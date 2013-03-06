\section{\texorpdfstring{@t{Banshee.CastleParser}}{Banshee.CastleParser}}
@Idx{Banshee.CastleParser}

Na parsování vstupního souboru s hradem použijeme opět knihovnu @t{parsec},
kterou jsme si popsali v sekci \ref{sec:krunimir-parser}.

\begin{code}
module Banshee.CastleParser(parseCastle) where
import Banshee.Castle

import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<*),(*>))
import Control.Monad
import Data.Array
\end{code}

Funkce @t{parseCastle} a typ @t{Parser} jsou definovány obdobně jako v
Krunimírovi:

@Idx{Banshee.CastleParser.parseCastle}
@Idx{Banshee.CastleParser.Parser}
\begin{code}
parseCastle :: String -> String -> Either ParseError Castle
parseCastle = parse castle

type Parser a = Parsec String () a
\end{code}

\subsection{\texorpdfstring{@t{SemiCastle}}{SemiCastle}}

Nyní si nadefinujeme typ @t{SemiCastle}, který budeme používat v průběhu
parsování. Tento typ představuje \uv{napůl známý} hrad. Pozice startu a televize
se dozvíme až v průběhu čtení souboru, proto na jejich uložení použijeme typ
@t{Maybe Loc}. Podobně na uložení políček nepoužijeme pole, ale seznam, jelikož
se informace o jednotlivých políčkách budeme dovídat postupně.

@Idx{Banshee.CastleParser.SemiCastle}
\begin{code}
data SemiCastle = SemiCastle
  { scFields :: [(Loc,Field)]
  , scTV :: Maybe Loc
  , scStart :: Maybe Loc
  , scScouts :: [Scout]
  }
\end{code}

@t{zeroCastle} je \uv{prázdný} (nebo nulový) hrad, o kterém zatím nevíme vůbec
nic, a @t{scAdd} je pomocná funkce, která do předaného @t{SemiCastle} přidá
jedno políčko.

@Idx{Banshee.CastleParser.zeroCastle}
@Idx{Banshee.CastleParser.scAdd}
\begin{code}
zeroCastle = SemiCastle
  { scFields = [] , scTV = Nothing 
  , scStart = Nothing , scScouts = [] }

scAdd :: Loc -> Field -> SemiCastle -> SemiCastle
scAdd loc field sc = sc { scFields = (loc,field):scFields sc }
\end{code}

\subsection{Jednotlivé parsery}

\subsubsection{Celý hrad}

Parser @t{castle} se stará o parsování celého hradu:

@Idx{Banshee.CastleParser.castle}
\begin{code}
castle :: Parser Castle
castle = do
  width <- read <$> (spaces *> many digit)
  height <- read <$> (spaces *> many digit)
  sc <- spaces >> foldM (row width) zeroCastle [1..height]
  eof
  tv <- case scTV sc of
    Just t -> return t
    Nothing -> parserFail "There was no television in the castle"
  start <- case scStart sc of
    Just s -> return s
    Nothing -> parserFail "There was no starting position in the castle"
  return $ Castle 
    { castleFields = array ((1,1),(width,height)) $ scFields sc
    , castleTV = tv, castleStart = start
    , castleScouts = scScouts sc }
\end{code}

Nejprve přečteme šířku (@t{width}) a výšku (@t{height}) hradu, která je zapsána
jako dvojice čísel na začátku souboru. Následně pomocí monadického kombinátoru
@t{foldM} a parseru @t{row}, který si brzy nadefinujeme, přečteme všechny řádky
ze souboru a získáme tak hodnotu @t{cs} typu @t{SemiCastle}, která reprezentuje
přečtený hrad.

Funkce @t{foldM} má typ @t{Monad m => (a -> b -> m a) -> a -> [b] -> m a} a je
analogická funkci @t{foldl}, ovšem pracuje s monádami.

Jakmile máme všechny řádky přečteny, ujistíme se, jestli jsme opravdu přečetli
pozici televizoru (@t{tv}) a startu (@t{start}). Pokud ne, pomocí @t{parserFail}
parsování ukončíme s chybou.  Pokud ano, nic nám už nebrání vytvořit hodnotu
@t{Castle} a vrátit ji.

\subsubsection{Řádek}

Na parsování jednoho řádku použijeme opět @t{foldM} a parser @t{field}, který si
vzápětí nadefinujeme a který přečte jedno políčko z mapy. Jako argumenty funkce
@t{row} musíme předat šířku mapy, @t{SemiCastle} který upravujeme a číslo řádku,
který čteme.

Na konci řádku by měl být znak konce řádku, náš parser ovšem akceptuje jakékoli
prázdné znaky.

@Idx{Banshee.CastleParser.row}
\begin{code}
row :: Int -> SemiCastle -> Int -> Parser SemiCastle
row width sc y = foldM (field y) sc [1..width] <* spaces
\end{code}

\subsubsection{Políčko}

Nyní už se dostáváme k nejdůležitější části našeho parseru -- čtení jednoho
políčka:

@Idx{Banshee.CastleParser.field}
\begin{code}
field :: Int -> SemiCastle -> Int -> Parser SemiCastle
field y sc x = free <|> wall <|> start <|> tv <|> scout
  where
    free  = char '.' >> return (scAdd (x,y) Free sc)
    wall  = char 'X' >> return (scAdd (x,y) Wall sc)
    start = char '&' >> case scStart sc of
      Just (x',y') -> parserFail $ "There is already starting position at " ++ show (x',y')
      Nothing -> return $ scAdd (x,y) Free sc { scStart = Just (x,y) }
    tv    = char '#' >> case scTV sc of
      Just (x',y') -> parserFail $ "There is already television at " ++ show (x',y')
      Nothing -> return $ scAdd (x,y) Free sc { scTV = Just (x,y) }
    scout = char '@' >> do
      moves <- many move
      let scout = if null moves 
            then [(x,y)]
            else applyMoves (x,y) $ init moves
      return $ scAdd (x,y) Free sc { scScouts = scout:scScouts sc }
\end{code}

Pro každý typ políčka jsme si nadefinovali vlastní pomocný parser. Prázdná pole
(parser @t{free}) a zdi (@t{wall}) jsou jednoduché, pouze do hradu přidáme jedno
pole. 

Narazíme-li na políčko s televizorem (@t{tv}) nebo startovní pozicí (@t{start}),
nejprve zkontrolujeme, jestli už jsme příslušnou pozici jednou nenačetli, a
pokud ano, skončíme parsování s chybou; v opačném případě upravíme odpovídající
část @t{SemiCastle}.

Posledním typem políčka je zvěd (parser @t{scout}). Za znakem `@t{@@}' následuje
několik pohybů, ze kterých musíme spočítat zvědovu trasu. Pokud za zvědem není
zadán ani jeden pohyb, jeho trasa je prostá -- stojí stále na své původní
pozici.

Pokud je zadáno pohybů více, pomocí funkce @t{applyMoves}, kterou si vzápětí
nadefinujeme, pohyby převedeme na seznam pozic. Ze seznamu pohybů ovšem předáme
všechny pohyby kromě posledního (funkcí @t{init}), protože v posledním kroku
svého cyklu se zvěd vždy přesune na svou počáteční pozici (tuto vlastnost
nekontrolujeme, ale tímto si ji vynutíme).

\subsection{Pohyby}

Pro reprezentaci pohybů využijeme jednoduchý algebraický datový typ:
    
@Idx{Banshee.CastleParser.Move}
\begin{code}
data Move = North | West | South | East deriving (Eq,Show)
\end{code}

Parser @t{move}, který jsme využili při čtení zvěda, vrací hodnoty tohoto typu
podle přečteného znaku:

@Idx{Banshee.CastleParser.move}
\begin{code}
move :: Parser Move
move = char '^' >> return North
   <|> char '<' >> return West
   <|> char 'v' >> return South
   <|> char '>' >> return East
\end{code}

Funkcí @t{applyMoves} pak \uv{aplikujeme} seznam pohybů na počáteční pozici,
čímž dostaneme seznam pozic:

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
