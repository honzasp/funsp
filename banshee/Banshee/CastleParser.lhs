\section{\texorpdfstring{@t{Banshee.CastleParser}}{Banshee.CastleParser}}
@Idx{Banshee.CastleParser}

\begin{code}
module Banshee.CastleParser(parseCastle) where
import Banshee.Castle

import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<$),(<*),(*>))
import Control.Monad
import Data.Array
\end{code}

@Idx{Banshee.CastleParser.parseCastle}
\begin{code}
parseCastle :: String -> String -> Either ParseError Castle
parseCastle = parse castle
\end{code}

@Idx{Banshee.CastleParser.Parser}
\begin{code}
type Parser a = Parsec String () a
\end{code}

@Idx{Banshee.CastleParser.SemiCastle}
\begin{code}
data SemiCastle = SemiCastle
  { scFields :: [(Loc,Field)]
  , scTV :: Maybe Loc
  , scStart :: Maybe Loc
  , scScouts :: [Scout]
  }
\end{code}

@Idx{Banshee.CastleParser.zeroCastle}
@Idx{Banshee.CastleParser.scAdd}
\begin{code}
zeroCastle = SemiCastle
  { scFields = [] , scTV = Nothing 
  , scStart = Nothing , scScouts = [] }

scAdd :: Loc -> Field -> SemiCastle -> SemiCastle
scAdd loc field sc = sc { scFields = (loc,field):scFields sc }
\end{code}

@Idx{Banshee.CastleParser.castle}
\begin{code}
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
\end{code}

@Idx{Banshee.CastleParser.row}
\begin{code}
row :: Int -> SemiCastle -> Int -> Parser SemiCastle
row width sc y = foldM (field y) sc [1..width] <* spaces
\end{code}

@Idx{Banshee.CastleParser.field}
\begin{code}
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
      let scout = if null moves 
            then [(x,y)]
            else applyMoves (x,y) $ init moves
      return $ scAdd (x,y) Free sc { scScouts = scout:scScouts sc }
\end{code}
    
@Idx{Banshee.CastleParser.Move}
@Idx{Banshee.CastleParser.move}
\begin{code}
data Move = North | West | South | East deriving (Eq,Show)

move = North <$ char '^'
   <|> West  <$ char '<'
   <|> South <$ char 'v'
   <|> East  <$ char '>'
\end{code}

@Idx{Banshee.CastleParser.applyMoves}
\begin{code}
applyMoves :: Loc -> [Move] -> [Loc]
applyMoves (x,y) []     = (x,y):[]
applyMoves (x,y) (m:ms) = (x,y):case m of
  North -> applyMoves (x,y-1) ms
  West  -> applyMoves (x-1,y) ms
  South -> applyMoves (x,y+1) ms
  East  -> applyMoves (x+1,y) ms
\end{code}
