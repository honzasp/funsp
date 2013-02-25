module Banshee.CastleParser where
import Banshee.Castle

import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<$),(<*),(*>))
import Control.Monad
import Data.Array

parseCastle :: String -> String -> Either ParseError Castle
parseCastle = parse castle

type Parser a = Parsec String () a

data SemiCastle = SemiCastle
  { scFields :: [(Loc,Field)]
  , scTV :: Maybe Loc
  , scStart :: Maybe Loc
  , scScouts :: [Scout]
  }

zeroCastle = SemiCastle
  { scFields = [] , scTV = Nothing 
  , scStart = Nothing , scScouts = [] }

scAdd :: Loc -> Field -> SemiCastle -> SemiCastle
scAdd loc field sc = sc { scFields = (loc,field):scFields sc }

castle :: Parser Castle
castle = do
  width <- read <$> (spaces *> many digit)
  height <- read <$> (spaces *> many digit)
  sc <- spaces >> foldM (row width) zeroCastle [1..height]
  eof
  tv <- case scTV sc of
    Just tv -> return tv
    Nothing -> parserFail "There was no television in the castle"
  start <- case scStart sc of
    Just st -> return st
    Nothing -> parserFail "There was no starting position in the castle"
  return $ Castle 
    { castleFields = array ((1,1),(width,height)) $ scFields sc
    , castleTV = tv, castleStart = start
    , castleScouts = scScouts sc }

row :: Int -> SemiCastle -> Int -> Parser SemiCastle
row width sc y = foldM (field y) sc [1..width] <* spaces

field :: Int -> SemiCastle -> Int -> Parser SemiCastle
field y sc x = free <|> wall <|> start <|> tv <|> scout
  where
    free  = char '.' >> return (scAdd (x,y) Free sc)
    wall  = char 'X' >> return (scAdd (x,y) Wall sc)
    start = char '&' >> case scStart sc of
      Just (x',y') -> parserFail $ "There is already starting position at " 
          ++ show x' ++ " " ++ show y'
      Nothing -> return $ scAdd (x,y) Free sc { scStart = Just (x,y) }
    tv    = char '#' >> case scTV sc of
      Just (x',y') -> parserFail $ "There is already television at "
          ++ show x' ++ " " ++ show y'
      Nothing -> return $ scAdd (x,y) Free sc { scTV = Just (x,y) }
    scout = char '@' >> do
      moves <- many move
      let scout = Scout $ applyMoves (x,y) moves
      return $ scAdd (x,y) Free sc { scScouts = scout:scScouts sc }
    
data Move = North | West | South | East deriving (Eq,Show)

move = North <$ char '^'
   <|> West  <$ char '<'
   <|> South <$ char 'v'
   <|> East  <$ char '>'

applyMoves :: Loc -> [Move] -> [Loc]
applyMoves (x,y) []     = (x,y):[]
applyMoves (x,y) (m:ms) = (x,y):case m of
  North -> applyMoves (x,y-1) ms
  West  -> applyMoves (x-1,y) ms
  South -> applyMoves (x,y+1) ms
  East  -> applyMoves (x+1,y) ms
