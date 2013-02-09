\section{@t{Krunimir.Parser}}
@Idx{Krunimir.Parser}

Pro syntaktickou analýzu (\uv{parsování}) použijeme knihovnu @t{parsec}.
Jedná se o \textit{de-facto} standardní nástroj na tvorbu parserů v Haskellu.

Narozdíl od generátorů jako GNU Bison, které vyžadují speciální soubor s
definicí gramatiky a strojově jej překládají do cílového jazyka, se
@t{parsec} používá v normálním kódu Haskellu a parsery se konstruují pomocí
\emph{vysokoúrovňových kombinátorů}, které umožňují kombinovat malé parsovací
funkce do větších celků.

% TODO: odkazy na web, literaturu o parsecu.
% TODO: nemáme lexer, co to je PEG

\begin{code}
module Krunimir.Parser (Krunimir.Parser.parse) where
import Text.Parsec 
import Control.Applicative ((<$>), (<$), (<*), (*>), (<*>))
import Krunimir.Ast
\end{code}

\subsection{Základní definice}

Parsery v knihovně @t{parsec} mají typ @t{Parsec s u a}, kde:

\begin{itemize}
\item @t{s} je typ vstupu, v našem případě @t{String}.
\item @t{u} je typ uživatelského stavu, tj. data, která uživatel
  (programátor parserů) může ukládat během parsování. My tuto vlastnost
  využívat nebudeme, proto použijeme \uv{prázdný} typ @t{()}.
\item @t{a} je výsledek parseru, tedy typ který parser vrátí. Naše parsery
  budou vracet více typů, např. příkaz (@t{Stmt}) nebo číslo
  (@t{Integer}), takže tento typ budeme předávat jako parametr typovému
  konstruktoru.
\end{itemize}

Abychom nemuseli neustále opakovat @t{Parsec String () ...}, nadefinujeme
\emph{typový synonym}, který využijeme i při prezentaci jednotlivých
kombinátorů.

@Idx{Krunimir.Parser.Parser}
\begin{code}
type Parser a = Parsec String () a
\end{code}

\subsection{Představení základních kombinátorů}

Některé kombinátory definuje přímo @t{parsec}:

\begin{description}
\item[@t{char :: Char -> Parser Char}] \hfill \\
  @t{char c} vytvoří parser, který akceptuje znak @t{c} a v případě
  úspěchu jej vrátí.

\item[@t{string :: [Char] -> Parser [Char]}] \hfill \\
  @t{string cs} je parser, jenž akceptuje sekvenci znaků (řetězec)
  @t{cs}.

\item[@t{(<|>) :: Parser a -> Parser a -> Parser a}] \hfill \\
  @t{p <|> q} představuje volbu -- nejprve aplikuje @t{p}, a
  pokud selže \emph{aniž zkonzumoval nějaký vstup}, aplikuje @t{q}.

\item[@t{(<?>) :: Parser a -> String -> Parser a}] \hfill \\
  @t{p <?> msg} aplikuje parser @t{p}, a pokud selže \emph{aniž
  zkonzumoval část vstupu}, nahradí část @t{"Expected ..."} chybové zprávy
  řetězcem @t{msg}.

\item[@t{try :: Parser a -> Parser a}] \hfill \\
  @t{try p} funguje jako @t{p}, ale s tím rozdílem, že pokud
  @t{p} selže, předstírá, že nic nezkonzumoval. Použijeme ho nejčastěji ve
  spojení s @t{<|>}.

\item[@t{many :: Parser a -> Parser [a]}] \hfill \\
  @t{many p} aplikuje parser @t{p} \emph{nula} či vícekrát a vrátí
  seznam výsledků z @t{p} (což znamená, že pokud @t{p} poprvé skončí
  neúspěchem, @t{many} vrátí prázdný seznam).

\item[@t{many1 :: Parser a -> Parser [a]}] \hfill \\
  @t{many1 p} funguje obdobně jako @t{many p}, s tím rozdílem, že
  @t{p} aplikuje \emph{alespoň jednou} (pokud @t{p} napoprvé selže,
  skončí neúspěchem i @t{many1}).

\item[@t{sepBy :: Parser a -> Parser sep -> Parser [a]}] \hfill \\
  @t{p `sepBy` s} zparsuje \emph{nula} či více výskytů @t{p}
  oddělených @t{s}.\footnote{Zápis pomocí @t{`} je pouze syntaktický
  cukr, kterým můžeme zapsat infixově volání jakékoli funkce; jinak je
  ekvivalentí klasickému @t{sepBy p s}.} Obdobně jako u @t{many}
  existuje varianta @t{sepBy1}, která aplikuje @t{p} alespoň jednou.

\end{description}

Každý parser je samozřejmě \emph{monáda}, proto můžeme použít základní monadické
operace:

\begin{description}
\item[@t{(>>=) :: Parser a -> (a -> Parser b) -> Parser b}] \hfill \\
  @t{p >>= f} aplikuje parser @t{p} a jeho výsledek předá funkci
  @t{f}.

\item[@t{(>>) :: Parser a -> Parser b -> Parser a}] \hfill \\
  @t{p >> q} nejprve aplikuje parser @t{p}, jeho výsledek zahodí a
  aplikuje @t{q}.

\item[@t{return :: a -> Parser a}] \hfill \\
  @t{return x} vytvoří parser, který vždy uspěje a vrátí @t{x}.

\end{description}

Každá monáda je \emph{aplikativní funktor}, tudíž můžeme použít i následující
operace:

\begin{description}
\item[@t{(<\$>) :: (a -> b) -> Parser a -> Parser b}] \hfill \\
  @t{f <\$> p} aplikuje parser @t{p} a v případě úspěchu předá jeho
  výsledek funkci @t{f}, jejíž výstup se stane výsledkem.

\item[@t{(<*>) :: Parser (a -> b) -> Parser a -> Parser b}] \hfill \\
  @t{p <*> q} nejprve aplikuje @t{p}, poté @t{q} a výsledek
  @t{q} předá funkci získané z @t{p}, jejíž výstup je výsledem.

\item[@t{(<\$) :: a -> Parser b -> Parser a}] \hfill \\
  @t{x <\$ p} aplikuje parser @t{p}, ale jeho výsledek zahodí a namísto
  toho vrátí @t{x}.

\item[@t{(<*) :: Parser a -> Parser b -> Parser a}] \hfill \\
  @t{p <* q} aplikuje nejprve parser @t{p}, poté parser @t{q},
  jehož výsledek zahodí a vrátí výsledek @t{p}.

\item[@t{(*>) :: Parser a -> Parser b -> Parser b}] \hfill \\
  @t{p *> q} aplikuje parser @t{p}, poté @t{q}, jehož výsledek
  vrátí. Tato funkce je ekvivalentní s @t{>>}, ale použití spolu s
  @t{<*} dáme přednost této variantě.\footnote{Všiměte si, že každý z
  operátorů @t{<*} nebo @t{*>} \uv{ukazuje} na ten parser, jehož
  hodnota bude vrácena.}

\end{description}

\subsection{Funkce @t{parse}}
@Idx{Krunimir.Parser.parse}

Funkce @t{parse} představuje \uv{uživatelské rozhraní} modulu
@t{Krunimir.Parser}. Vstupem je jméno parsovaného souboru (použije se v
případných chybových hláškách) a samotný text programu. Výstupem je buď chyba
(@t{ParseError}) nebo želví program (@t{Program}).

Využijeme stejně pojmenovanou funkci, kterou nám @t{parsec} nabízí, a
předáme jí nejprve parser celého programu (@t{program}) a pak oba zbývající
argumenty.

\begin{code}
parse :: String -> String -> Either ParseError Program
parse filename txt =
  Text.Parsec.parse program filename txt
\end{code}

\subsection{Programy}

Na začátku programu může být libovolné množství prázdných znaků\, následuje nula a více top-příkazů
a konec souboru.

@Idx{Krunimir.Parser.program}
\begin{code}
program :: Parser Program
program = spaces *> many topStmt <* eof
\end{code}

Operátory @t{*>} a @t{<*} mají stejnou prioritu a jsou asociativní
zleva, což znamená že tento kód je ekvivalentní @t{(spaces *> many topStmt)
<* eof}. Nejprve se tedy aplikuje @t{spaces},\footnote{Parser @t{spaces} definuje samotná
knihovna @t{parsec}, má typ @t{Parser ()} a zahodí nula a více
prázdných znaků.} jehož výsledek se zahodí, poté @t{many topStmt}, kterým
získáme seznam top-příkazů, a nakonec @t{eof}. Pokud @t{eof} uspěje,
dostaneme výsledek z @t{many topStmt}, pokud ne, parser vrátí chybu.

\subsubsection{Top-příkazy}

Top-příkaz je buď definice procedury (parser @t{define}) nebo příkaz
(parser @t{stmt}), ze kterých pomocí příslušných datových konstruktorů
(@t{TopDefine}, resp. @t{TopStmt}) vytvoříme typ
@t{TopStmt}.\footnote{
Všimněte si, že identifikátor @t{TopStmt} může označovat dvě odlišné entity
-- \emph{typový} konstruktor @t{TopStmt} (v deklaraci @t{topStmt ::
Parser \emph{TopStmt}}) a \emph{datový} konstruktor @t{TopStmt}
příslušející stejnojmennému typu (ve výrazu @t{\emph{TopStmt} <\$> stmt}).
V Haskellu se s takovýmito případy, kdy definujeme datový typ se stejnojmenným
konstruktorem, setkáváme poměrně často.}

@Idx{Krunimir.Parser.topStmt}
\begin{code}
topStmt :: Parser TopStmt
topStmt = 
  TopDefine <$> try define <|>
  TopStmt <$> stmt
\end{code}

\subsubsection{Definice procedur}

Definice procedur v Krunimírově jazyku začínají klíčovým slovem @t{define}
následovaným jménem procedury, za kterým je v závorkách nula a více parametrů.
Tělo procedury je uzavřeno ve složených závorkách.

@Idx{Krunimir.Parser.define}
\begin{code}
define :: Parser Define
define = do
  string "define" >> skipMany1 space
  name <- identifier
  params <- parens $ identifier `sepBy` comma
  stmts <- braces $ many stmt
  return $ Define name params stmts
\end{code}

Použili jsme pomocné funkce @t{parens} a @t{braces}, které slouží k
\uv{obalování závorkami} a které si nadefinujeme později.

\subsection{Příkazy}

K parsování \emph{příkazů} slouží @t{stmt}, která jen aplikuje další
pomocné parsery a pojmenuje případnou chybu.

@Idx{Krunimir.Parser.stmt}
\begin{code}
stmt :: Parser Stmt
stmt =
  try repeatStmt <|>
  try ifStmt <|>
  try splitStmt <|>
  try procStmt <?>
  "statement"
\end{code}

\subsubsection{Volání procedur}

Začneme syntaxí užitou při volání procedur. Jak zabudované primitivní
(@t{forward}, @t{color}...), tak programátorem definované procedury se
volají stejně, proto je musíme rozlišit podle jména a podle toho vytvořit
příslušný uzel syntaktického stromu.

Volání začíná jménem volané procedury a následuje v závorkách seznam argumentů,
který může být prázdný, závorky ale vynechat nelze.

@Idx{Krunimir.Parser.procStmt}
\begin{code}
procStmt :: Parser Stmt
procStmt = do
  name <- identifier
  args <- parens $ expr `sepBy` comma
  case name of
    "forward" -> primitive ForwardStmt "forward" args
    "left"    -> primitive LeftStmt "left" args
    "right"   -> primitive RightStmt "right" args
    "pen"     -> primitive PenStmt "pen" args
    "color"   -> case args of
        [r,g,b] -> return $ ColorStmt r g b
        _       -> parserFail $
            "color takes 3 arguments, got " ++ show (length args)
    _ -> return $ CallStmt name args
  where
    primitive con _ [arg] = return $ con arg
    primitive _ name args = parserFail $
        name ++ " takes 1 argument, got " ++ show (length args)
\end{code}

Využili jsme parsery @t{identifier} a @t{parens}, které si
nadefinujeme později, a pomocnou funkci @t{primitive}, kterou si ušetříme
opakování při zpracování příkazů @t{forward}, @t{left}, @t{right}
a @t{pen}, které všechny vyžadují jeden argument.

\subsubsection{Příkazy @t{if} a @t{repeat}}

Syntaxe pro @t{if} a @t{repeat} je velmi podobná -- nejprve klíčové
slovo, poté v závorkách výraz a nakonec seznam příkazů ve složených závorkách.

@Idx{Krunimir.Parser.repeatStmt}
@Idx{Krunimir.Parser.ifStmt}
\begin{code}
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
\end{code}

Pomocný parser @t{keyword} nadefinujeme později; kdybychom místo něj
použili jednoduše @t{string}, například @t{string "if"}, a programátor
by nadefinoval třeba proceduru @t{iffy} a pokusil by se ji zavolat
(@t{iffy(42)}), parser by přečetl pouze @t{"if"}, domníval by se, že
jde o příkaz @t{if}, a pak nevěděl co s @t{"fy(42)"}, protože očekává
otevírací závorku. Naproti tomu @t{keyword "if"} se aplikuje pouze na
sekvenci znaků @t{"if"} za kterou \emph{nenásleduje písmenko}, čímž
zajistíme, že jsme opravdu narazili na celé slovo @t{if}.

\subsubsection{Konstrukce @t{split}}

Syntaxe pro @t{split} je přímočará, za klíčovým slovem následují rovnou
složené závorky se seznamem příkazů.

@Idx{Krunimir.Parser.splitStmt}
\begin{code}
splitStmt :: Parser Stmt
splitStmt = do
  keyword "split"
  stmts <- braces $ many stmt
  return $ SplitStmt stmts
\end{code}

\subsection{Výrazy}

Parsování \emph{výrazů} je o něco složitější, jelikož se musíme vypořádat s
prioritami a asociativitami jednotlivých operátorů.

Gramatiku našich výrazů můžeme zapsat v BNF formě jako

\begin{grammar}
<expr> ::= <add-expr>

<add-expr> ::= <add-expr> add-op <neg-expr>
\alt <neg-expr>

<neg-expr> ::= "-" <mul-expr>
\alt <mul-expr>

<mul-expr> ::= <mul-expr> mul-op <a-expr>
\alt <a-expr>

<a-expr> ::= variable
\alt integer
\alt "(" <expr> ")"
\end{grammar}

Problém je, že pravidla pro sčítání/odčítání a násobení/dělení jsou rekurzivní
zleva, takže je nelze zpracovávat pomocí gramatiky PEG. Proto je musíme
přeformulovat do podoby

\begin{grammar}
<expr>      = <add-expr>

<add-expr>  = <neg-expr> (add-op <neg-expr>)*

<neg-expr>  = "-"? <mul-expr>

<mul-expr>  = <a-expr> (mul-op <a-expr>)*

<a-expr>    = variable | integer | "(" <expr> ")"
\end{grammar}

Tuto PEG gramatiku již můžeme použít, ale struktura gramatiky již neodpovídá
struktuře syntaktického stromu. @t{parsec} naštěstí obsahuje pomocné
funkce, které nám úkol značně ulehčí. 

Použijeme funkci @t{chainl1}, jejíž typ je @t{chainl1 :: Parser a ->
Parser (a -> a -> a) -> Parser a}. @t{chainl p op} zparsuje jeden a více
výskytů @t{p} oddělených @t{op}. Výsledky z @t{p} postupně odleva
\uv{spojí} pomocí funkcí vrácených z @t{op}.

@Idx{Krunimir.Parser.expr}
% indexovat i pomocne dilci vyrazy?
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

\marginnote{Tady je příklad téměř nutností, bez něj nelze chainl1 pochopit.}

\subsection{Pomocné parsery}

Nakonec si nadefinujeme drobné parsery, které jsme použili. Každý z nich
zkonzumuje i všechny prázdné znaky, které se za ním nachází, takže se s jejich
ošetřením nemusíme zabývat ve \uv{vyšších} parserech.

V identifikátorech povolíme i velká písmena a číslice, pokud se nenachází na
začátku.

@Idx{Krunimir.Parser.integer}
@Idx{Krunimir.Parser.identifier}
@Idx{Krunimir.Parser.keyword}
@Idx{Krunimir.Parser.parens}
@Idx{Krunimir.Parser.braces}
\begin{code}
integer :: Parser Integer
integer = read <$> many1 digit <* spaces
identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum <* spaces

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
\marginnote{Vysvětlit between?}
