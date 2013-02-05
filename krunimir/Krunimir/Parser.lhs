\section{\texttt{Krunimir.Parser}}

Pro syntaktickou analýzu (\uv{parsování}) použijeme knihovnu \texttt{parsec}.
Jedná se o \textit{de-facto} standardní nástroj na tvorbu parserů v Haskellu.

Narozdíl od generátorů jako GNU Bison, které vyžadují speciální soubor s
definicí gramatiky a strojově jej překládají do cílového jazyka, se
\texttt{parsec} používá v normálním kódu Haskellu a parsery se konstruují pomocí
\emph{vysokoúrovňových kombinátorů}, které umožňují kombinovat malé parsovací
funkce do větších celků.

% TODO: odkazy na web, literaturu o parsecu.
% TODO: nemáme lexer, co to je PEG

\subsection{Základní definice}

\begin{code}
{-# LANGUAGE PatternGuards #-}
module Krunimir.Parser (Krunimir.Parser.parse) where
import Text.Parsec 
import Control.Applicative ((<$>), (<$), (<*), (*>), (<*>))
import Krunimir.Ast
\end{code}

Parsery v knihovně \texttt{parsec} mají typ \texttt{Parsec s u a}, kde:

\begin{itemize}
\item \texttt{s} je typ vstupu, v našem případě \texttt{String}.
\item \texttt{u} je typ uživatelského stavu, tj. data, která uživatel
  (programátor parserů) může ukládat během parsování. My tuto vlastnost
  využívat nebudeme, proto použijeme \uv{prázdný} typ \texttt{()}.
\item \texttt{a} je výsledek parseru, tedy typ který parser vrátí. Naše parsery
  budou vracet více typů, např. příkaz (\texttt{Stmt}) nebo číslo
  (\texttt{Integer}), takže tento typ budeme předávat jako parametr typovému
  konstruktoru.
\end{itemize}

Abychom nemuseli neustále opakovat \texttt{Parsec String () ...}, nadefinujeme
\emph{typový synonym}

\begin{code}
type Parser a = Parsec String () a
\end{code}

\subsection{Představení základních kombinátorů}

Některé kombinátory definuje přímo \texttt{parsec}:

\begin{description}
\item[\texttt{(<|>) :: Parser a -> Parser a -> Parser a}] \hfill \\
  implementuje možnost volby. \texttt{p <|> q} nejprve aplikuje \texttt{p}, a
  pokud selže \emph{aniž zkonzumoval nějaký vstup}, aplikuje \texttt{q}.

\item[\texttt{try :: Parser a -> Parser a}] \hfill \\
  \texttt{try p} funguje jako \texttt{p}, ale s tím rozdílem, že pokud
  \texttt{p} selže, předstírá, že nic nezkonzumoval. Použijeme ho nejčastěji ve
  spojení s \texttt{<|>}.

\end{description}

Každý parser je samozřejmě \emph{monáda}, proto můžeme použít základní monadické
operace:

\begin{description}
\item[\texttt{(>>=) :: Parser a -> (a -> Parser b) -> Parser b}] \hfill \\
  \texttt{p >>= f} aplikuje parser \texttt{p} a jeho výsledek předá funkci
  \texttt{f}.

\item[\texttt{(>>) :: Parser a -> Parser b -> Parser a}] \hfill \\
  \texttt{p >> q} nejprve aplikuje parser \texttt{p}, jeho výsledek zahodí a
  aplikuje \texttt{q}.
\end{description}

Každá monáda je \emph{aplikativní funktor}, tudíž můžeme použít i následující
operace:

\begin{description}
\item[\texttt{(<\$>) :: (a -> b) -> Parser a -> Parser b}] \hfill \\
  \texttt{f <\$> p} aplikuje parser \texttt{p} a v případě úspěchu předá jeho
  výsledek funkci \texttt{f}, jejíž výstup se stane výsledkem.

\item[\texttt{(<*>) :: Parser (a -> b) -> Parser a -> Parser b}] \hfill \\
  \texttt{p <*> q} nejprve aplikuje \texttt{p}, poté \texttt{q} a výsledek
  \texttt{q} předá funkci získané z \texttt{p}, jejíž výstup je výsledem.

\item[\texttt{(<\$) :: a -> Parser b -> Parser a}] \hfill \\
  \texttt{x <\$ p} aplikuje parser \texttt{p}, ale jeho výsledek zahodí a namísto
  toho vrátí \texttt{x}.

\item[\texttt{(<*) :: Parser a -> Parser b -> Parser a}] \hfill \\
  \texttt{p <* q} aplikuje nejprve parser \texttt{p}, poté parser \texttt{q},
  jehož výsledek zahodí a vrátí výsledek \texttt{p}.

\item[\texttt{(*>) :: Parser a -> Parser b -> Parser b}] \hfill \\
  \texttt{p *> q} aplikuje parser \texttt{p}, poté \texttt{q}, jehož výsledek
  vrátí. Tato funkce je ekvivalentní s \texttt{>>}, ale použití spolu s
  \texttt{<*} dáme přednost této variantě.\footnote{Všiměte si, že každý z
  operátorů \texttt{<*} nebo \texttt{*>} \uv{ukazuje} na ten parser, jehož
  hodnota bude vrácena.}

\end{description}

\subsection{Funkce \texttt{parse}}

Funkce \texttt{parse} představuje \uv{uživatelské rozhraní} modulu
\texttt{Krunimir.Parser}. Vstupem je jméno parsovaného souboru (použije se v
případných chybových hláškách) a samotný text programu. Výstupem je buď chyba
(\texttt{ParseError}) nebo želví program (\texttt{Program}).

Využijeme stejně pojmenovanou funkci, kterou nám \texttt{parsec} nabízí, a
předáme jí nejprve parser celého programu (\texttt{program}) a pak oba zbývající
argumenty.

\begin{code}
parse :: String -> String -> Either ParseError Program
parse filename txt =
  Text.Parsec.parse program filename txt
\end{code}

\subsection{Příkazy}

Na začátku programu může být libovolné množství prázdných znaků\, následuje nula a více top-příkazů
a konec souboru.

\begin{code}
program :: Parser Program
program = spaces *> many topStmt <* eof
\end{code}

Operátory \texttt{*>} a \texttt{<*} mají stejnou prioritu a jsou asociativní
zleva, což znamená že tento kód je ekvivalentní \texttt{(spaces *> many topStmt)
<* eof}. Nejprve se tedy aplikuje \texttt{spaces},\footnote{Parser \texttt{spaces} definuje samotná
knihovna \texttt{parsec}, má typ \texttt{Parser ()} a zahodí nula a více
prázdných znaků.} jehož výsledek se zahodí, poté \texttt{many topStmt}, kterým
získáme seznam top-příkazů, a nakonec \texttt{eof}. Pokud \texttt{eof} uspěje,
dostaneme výsledek z \texttt{many topStmt}, pokud ne, parser vrátí chybu.

\begin{code}
topStmt :: Parser TopStmt
topStmt = 
  TopDefine <$> try define <|>
  TopStmt <$> stmt

define :: Parser Define
define = do
  string "define" >> skipMany space
  name <- many1 lower
  params <- parens $ sepBy identifier comma
  stmts <- braces $ many stmt
  return $ Define name params stmts

stmt :: Parser Stmt
stmt =
  try repeatStmt <|>
  try ifStmt <|>
  try splitStmt <|>
  try funStmt <?>
  "statement"

funStmt :: Parser Stmt
funStmt = do
  name <- identifier
  args <- parens $ sepBy expr comma
  case name of
    "forward" -> primitive ForwardStmt "forward" args
    "left"    -> primitive LeftStmt "left" args
    "right"   -> primitive RightStmt "right" args
    "pen"     -> primitive PenStmt "pen" args
    "color"
      | [r,g,b] <- args -> return $ ColorStmt r g b
      | otherwise       -> parserFail $ "color takes 3 arguments, got " ++ show (length args)
    _ -> return $ CallStmt name args
  where
    primitive con name args 
      | [arg] <- args = return $ con arg
      | otherwise     = parserFail $ name ++ " takes 1 argument, got " ++ show (length args)

repeatStmt :: Parser Stmt
repeatStmt = do
  keyword "repeat"
  times <- parens expr
  stmts <- braces $ many stmt
  return $ RepeatStmt times stmts

ifStmt :: Parser Stmt
ifStmt = do
  keyword "if"
  cond <- parens expr
  stmts <- braces $ many stmt
  return $ IfStmt cond stmts

splitStmt :: Parser Stmt
splitStmt = do
  keyword "split"
  stmts <- braces $ many stmt
  return $ SplitStmt stmts

\end{code}

\subsection{Výrazy}

\begin{code}
expr :: Parser Expr
expr = addExpr <?> "expression"

addExpr,mulExpr,negExpr,aExpr,varExpr,litExpr :: Parser Expr

addExpr = chainl1 mulExpr (addOp <* spaces)
mulExpr = chainl1 negExpr (mulOp <* spaces)

addOp,mulOp :: Parser (Expr -> Expr -> Expr)
addOp = 
  BinopExpr AddOp <$ char '+' <|>
  BinopExpr SubOp <$ char '-' 

mulOp =
  BinopExpr MulOp <$ char '*' <|>
  BinopExpr DivOp <$ char '/' 

negExpr = (negOp <* spaces) <*> aExpr <|> aExpr

negOp :: Parser (Expr -> Expr)
negOp = NegateExpr <$ char '-'

aExpr = litExpr <|> varExpr <|> parens expr
varExpr = VariableExpr <$> identifier
litExpr = LiteralExpr <$> integer

\end{code}

\subsection{Pomocné parsery}

\begin{code}
integer :: Parser Integer
integer = read <$> many1 digit <* spaces
identifier :: Parser String
identifier = many1 lower <* spaces

keyword :: String -> Parser ()
keyword s = string s >> notFollowedBy alphaNum >> spaces

lparen,rparen,lbrace,rbrace,comma :: Parser ()
lparen = char '(' >> spaces
rparen = char ')' >> spaces
lbrace = char '{' >> spaces
rbrace = char '}' >> spaces
comma = char ',' >> spaces

parens,braces :: Parser a -> Parser a
parens = between lparen rparen
braces = between lbrace rbrace
\end{code}
