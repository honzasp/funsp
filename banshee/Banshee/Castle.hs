module Banshee.Castle where
import Data.Array
import Control.Monad

data Castle = Castle
  { castleFields :: Array Loc Field
  , castleTV :: Loc
  , castleStart :: Loc
  , castleScouts :: [Scout]
  } 

type Scout = [Loc]
type Loc = (Int,Int)

data Field = Free | Wall deriving (Eq,Show)

newtype Slice = Slice (Array Loc SliceField)

data SliceField = FreeSF | WallSF | ScoutSF Loc deriving (Eq,Show) 

slices :: Castle -> [Slice]
slices castle = map slice [0..period-1] where
  fields = castleFields castle
  period = foldl lcm 1 . map length . castleScouts $ castle
  loopedScouts = map cycle $ castleScouts castle
  sfFields = fmap fieldToSF fields

  slice s = Slice $ sfFields // 
    [((x,y),ScoutSF next) 
    | scout <- loopedScouts
    , let (x,y):next:_ = drop s scout]

fieldToSF Free = FreeSF
fieldToSF Wall = WallSF

instance Show Castle where
  show castle = "Castle " ++ dimensions ++ "\n" ++ body ++ "\n" ++ scouts
    where
    ((1,1),(width,height)) = bounds $ castleFields castle
    dimensions = show width ++ "x" ++ show height
    body = unlines $ map ((' ':) . showRow) [1..height]
    showRow y = map (showField y) [1..width]
    showField y x = case castleFields castle ! (x,y) of
      Free -> '.'
      Wall -> 'X'

    scouts = unlines $ map showScout $ castleScouts castle
    showScout locs = show locs

instance Show Slice where
  show (Slice slice) = "Slice " ++ dimensions ++ "\n" ++ body
    where
    ((1,1),(width,height)) = bounds slice
    dimensions = show width ++ "x" ++ show height
    body = unlines $ map ((' ':) . showRow) [1..height]
    showRow y = map (showField y) [1..width]
    showField y x = case slice ! (x,y) of
      FreeSF -> '.'
      WallSF -> 'X'
      ScoutSF _ -> '@'
