module Banshee.Navigate where
import Data.Array
import Data.Array.ST
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.ST

import Banshee.Castle

data Path = Path Int [Loc] deriving Show

pathLength (Path len _) = len

moves :: Bool -> Slice -> Slice -> (Loc,Path) -> [(Loc,Path)]
moves thruWalls (Slice foreslice) (Slice afterslice) ((x,y),Path len ps) = 
  [((tox,toy),Path (len+1) $ (x,y):ps)
    | (tox,toy) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    , tox >= 1 && tox <= width
    , toy >= 1 && toy <= height
    , case afterslice ! (tox,toy) of
        FreeSF -> True
        WallSF -> thruWalls
        ScoutSF _ -> False
    , case foreslice ! (tox,toy) of
        ScoutSF scout | scout == (x,y) -> False
        _ -> True
    ]
  where
    ((1,1),(width,height)) = bounds foreslice

turn :: Castle -> [Slice] -> STArray s (Int,Loc) (Maybe Path) ->
  [(Loc,Path)] -> ST s (Either Path [(Loc,Path)])
turn castle slices bests = step 0 (cycle slices)
  where
  step _ _ [] = return $ Right []
  step t (slice1:slice2:slices) locpaths = do
    let (starts,rest) = span ((<= t) . pathLength . snd) locpaths
        offset = t `mod` period

    nextsss <- fmap catMaybes $ forM starts $ \((x,y),path) -> do
      uncov <- isNothing <$> readArray bests (offset,(x,y))
      if uncov then do
          writeArray bests (offset,(x,y)) $ Just path
          return . Just . (,) ((x,y),path) $ moves False slice1 slice2 ((x,y),path)
        else return Nothing
    let (starts',nextss) = unzip nextsss
    let nexts = concat nextss
    
    tv <- readArray bests (offset,castleTV castle)
    case tv of
      Nothing -> (fmap (starts'++)) <$> step (t+1) (slice2:slices) (nexts ++ rest)
      Just path -> return $ Left path

  period = length slices

navigate :: Castle -> [Slice] -> Bool -> Maybe [Loc]
navigate castle slices thruWalls = runST $ do
  let ((1,1),(width,height)) = bounds $ castleFields castle
      start = [(castleStart castle,Path 0 [])]
  bests <- newArray ((0,(1,1)),(length slices-1,(width,height))) Nothing
  result <- if thruWalls
    then wallStep bests start
    else turn castle slices bests start
  case result of
    Left (Path len locs) -> return . Just . reverse $ castleTV castle:locs
    Right _ -> return Nothing

  where
  wallStep :: STArray s (Int,Loc) (Maybe Path) -> [(Loc,Path)] ->
    ST s (Either Path [(Loc,Path)])
  wallStep bests locpaths = do
    result <- turn castle slices bests locpaths
    case result of
      Right locpaths'@(_:_) -> do
        let nextss = (`map` locpaths') $ \((x,y),path@(Path len _)) ->
              let offset = len `mod` period
                  offset' = (len+1) `mod` period
              in  moves True (slices !! offset) (slices !! offset') ((x,y),path)
        wallStep bests (concat nextss)
      _ -> return result

  period = length slices
