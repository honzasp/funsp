\section{\texorpdfstring{@t{Krunimir.Evaluator}}{Krunimir.Evaluator}}
@Idx{Krunimir.Evaluator}

Nyní se dostáváme k jádru problému, totiž samotnému \emph{vyhodnocování}
Krunimírova programu, implementovanému funkcí @t{eval}. Vstupem této funkce je
syntaktický strom v podobě typu @t{Program} (což je jen synonym pro
@t{[TopStmt]}), výstupem stopa želvy jako typ @t{Trace}.

\begin{code}
module Krunimir.Evaluator (eval) where
import Krunimir.Trace
import Krunimir.Ast

import qualified Data.Map as M
import Data.List (genericReplicate)
\end{code}

\subsection{Pomocné typy}

Definujeme si datový typ @t{Turtle}, který zahrnuje celý stav želvy -- její
pozici, natočení a barvu a tloušťku pera.\footnote{Typ @t{Float} jsme použili
namísto obvyklého @t{Double} pro ušetření paměti, měl by totiž vyžadovat pouze 4
bajty namísto 8. Jelikož Krunimírovy programy pracující s desetitisíci želvami
nejsou žádnou výjimkou, ušetřené bajty se na výkonu programu pozitivně projeví.}

@Idx{Krunimir.Evaluator.Turtle}
\begin{code}
data Turtle = Turtle
  { getPos :: (Float,Float)
  , getAngle :: Integer
  , getColor :: (Int,Int,Int)
  , getPen :: Int
  }
\end{code}

V průběhu vyhodnocování musíme mít uloženy informace o definovaných procedurách
a aktuálních hodnotách argumentů (proměnných). K tomu slouží typ @t{Env}.
Definujeme si synonyma @t{ProcMap} a @t{VarMap}, což jsou \emph{mapy} mapující
jména procedur na jejich definice a jména proměnných na hodnoty.

@Idx{Krunimir.Evaluator.Env}
@Idx{Krunimir.Evaluator.ProcMap}
@Idx{Krunimir.Evaluator.VarMap}
\begin{code}
data Env = Env ProcMap VarMap
type ProcMap = M.Map String Define
type VarMap = M.Map String Integer
\end{code}

\subsection{Představení \texorpdfstring{@t{DiffTrace}}{DiffTrace}}
@idx{Krunimir.Evaluator.DiffTrace}

Nyní se musíme rozhodnout, jak přesně budeme vyhodnocování stopy v syntaktickém
stromu implementovat. Určitě bude vhodné vytvořit funkci na vyhodnocení jednoho
příkazu a jednoho výrazu.

Jaké informace potřebujeme k tomu, abychom mohli spočítat hodnotu výrazu?
Vzhledem k tomu, že se ve výrazu můžou vyskytovat argumenty aktuální procedury,
budeme potřebovat @t{Env}, z něhož získáme hodnoty aktuálních proměnných
(argumentů). Z toho nám plyne typ pro funkci @t{evalExpr}:

@idx{Krunimir.Evaluator.evalExpr}
\begin{haskell}
evalExpr :: Env -> Expr -> Integer
\end{haskell}

Dále bude třeba vytvořit funkci, která vyhodnotí příkaz. Argumenty této funkce
bude učitě znovu @t{Env} (potřebujeme znát, jaké procedury existují) a
@t{Turtle} (musíme vědět, jaký je aktuální stav želvy). 

Jakou hodnotu bychom měli vrátit? Každý příkaz změní stav želvy, proto bychom
novou želvu měli vrátit jako část výsledku. Hlavní je ale to, jestli příkaz
nezmění stopu, kterou za sebou želva zanechá. Jakým způsobem ale tuto změnu
reprezentovat? Nemůžeme použít přímo typ @t{Trace}, jelikož ten reprezentuje
\emph{celou} želvinu trasu, kdežto my spočteme jen její \emph{začátek}, neboť
za tímto jedním příkazem mohou následovat další, které trasu rovněž prodlouží.

Nejlepší bude, když vrátíme \emph{funkci}, která jako argument dostane @t{Trace}
získaný z \emph{následujících} příkazů a vrátí novou @t{Trace}.

Tím získáváme typ:

\begin{haskell}
evalStmt :: Env -> Stmt -> Turtle -> (Turtle,Trace -> Trace)
\end{haskell}

S funkcemi typu @t{Trace -> Trace} budeme pracovat často, proto si vytvoříme
\emph{nový typ}.

@Idx{Krunimir.Evaluator.DiffTrace}
@Idx{Krunimir.Evaluator.applyDT}
\begin{code}
newtype DiffTrace = DiffTrace { applyDT :: Trace -> Trace }
\end{code}

Jaký typ bude mít funkce @t{applyDT}? Z hodnoty typu @t{DiffTrace} extrahuje
hodnotu typu @t{Trace -> Trace}, tudíž její typ bude @t{DiffTrace -> (Trace ->
Trace)}, neboli @t{DiffTrace -> Trace -> Trace}. To znamená, že na @t{applyDT}
můžeme nahlížet jako na funkci se dvěmi argumenty, která \emph{aplikuje}
změnu -- @t{DiffTrace} -- na @t{Trace}, čímž získáme novou @t{Trace}.

Nadefinujeme si také operaci @t{identity}, tj. žádná změna se neprovede.

@Idx{Krunimir.Evaluator.identityDT}
\begin{code}
identityDT :: DiffTrace
identityDT = DiffTrace { applyDT = id }
\end{code}

S tímto novým typem, který reprezentuje \emph{rozdíl} nebo \emph{změnu} stopy
@t{Trace}, bude typ funkce @t{evalStmt} vypadat takto:

@idx{Krunimir.Evaluator.evalStmt}
\begin{haskell}
evalStmt :: Env -> Stmt -> Turtle -> (Turtle,DiffTrace)
\end{haskell}

Tuto deklaraci typu můžeme chápat takto: \uv{@t{evalStmt} je funkce vyžadující
mapu procedur a proměnných uložených v typu @t{Env}, dále příkaz k vyhodnocení a
stav želvy; vrátí změněný stav želvy a \emph{změnu}, kterou tento příkaz vyvolá
na stopě želvy.}

I když toto typové kung-fu může vypadat na první pohled zbytečně komplikovaně a
složitě, opak je pravdou -- umožní nám vyhodnocování příkazů implementovat velmi
elegantně a jednoduše.

\subsection{Funkce \texorpdfstring{@t{eval}}{eval}}
@Idx{Krunimir.Evaluator.eval}

Nejprve představíme funkce @t{eval}, která vyhodnotí celý program:

\begin{code}
eval :: Program -> Trace
eval prog = applyDT (snd $ evalStmts env stmts startTurtle) EmptyTrace
  where

  (defs,stmts) = foldl go ([],[]) (reverse prog)
    where go (ds,ss) topstmt = case topstmt of
            TopDefine def -> (def:ds,ss)
            TopStmt stmt -> (ds,stmt:ss)

  env = Env procMap varMap
  procMap = M.fromList [(defineName def,def) | def <- defs]
  varMap = M.empty

  startTurtle = Turtle
    { getPos = (350.5,350.5)
    , getAngle = 0
    , getColor = (0,0,0)
    , getPen = 0
    }
\end{code}

Nejdříve si rozeberme klauzuli @t{where}. Nejprve průchodem seznamu @t{prog}
funkcí @t{foldl} získáme seznam definic @t{defs} a seznam příkazů @t{stmts},
které extrahujeme z top-příkazů.

Ze seznamu definic vytvoříme mapu procedur @t{procMap}. Na nejvyšší úrovni se v
programu nenachází žádné proměnné, proto je mapa proměnných prázdná.
@t{startTurtle} je počáteční stav želvy -- nachází se uprostřed obrázku s
vypnutým černým perem a je otočená směrem nahoru.

V samotném těle funkce @t{eval} nejprve vyhodnotíme pomocí funkce @t{evalStmts}
seznam příkazů, čímž získáme dvojici @t{(Turtle,DiffTrace)}. První prvek, želva,
nás nezajímá, ale funkcí @t{snd} získáme hodnotu @t{DiffTrace}, která
reprezentuje změnu, jenž program vykoná na celkové stopě želvy. Tuto změnu
aplikujeme na prázdnou stopu, takže získáme kýženou hodnotu @t{Trace}.

\subsection{Vyhodnocování příkazů}

Funkce @t{evalStmts}, která vyhodnotí seznam příkazů, vždy vyhodnotí jeden
příkaz, poté seznam následujících příkazů a vrátí výslednou želvu a složený
@t{DiffTrace}.

@Idx{Krunimir.Evaluator.evalStmts}
\begin{code}
evalStmts :: Env -> [Stmt] -> Turtle -> (Turtle,DiffTrace)
evalStmts _ [] turtle = (turtle,identityDT)
evalStmts env (stmt:stmts) turtle = 
  let (turtle',dt) = evalStmt env stmt turtle
      (turtle'',dt') = evalStmts env stmts turtle'
  in (turtle'',DiffTrace { applyDT = applyDT dt . applyDT dt' })
\end{code}

V @t{evalStmt} použijeme velký @t{case}, v němž patřičně reagujeme na každý druh
příkazu.

@Idx{Krunimir.Evaluator.evalStmt}
\begin{code}
evalStmt :: Env -> Stmt -> Turtle -> (Turtle,DiffTrace)
evalStmt env stmt = case stmt of
  ForwardStmt e -> forward (ee e)
  LeftStmt e    -> rotate (negate $ ee e)
  RightStmt e   -> rotate (ee e)
  PenStmt e     -> pen (ee e)
  ColorStmt r g b -> color (ee r) (ee g) (ee b)
  RepeatStmt e stmts -> evalStmts env $ concat $ genericReplicate (ee e) stmts
  IfStmt e stmts -> if ee e > 0 then evalStmts env stmts else noop
  SplitStmt stmts -> split $ evalStmts env stmts
  CallStmt name args -> let
      def = lookupDef env name
      binds = zip (defineParams def) (map ee args)
      newenv = makeEnv env binds
    in evalStmts newenv (defineStmts def)
  where ee = evalExpr env
\end{code}

Primitivní operace s želvou jsme ošetřili pomocnými funkcemi, které
implementujeme později. Podmínka a cyklus v podobě @t{RepeatStmt} a @t{IfStmt}
jsou implementovány pomocí @t{evalStmts}, stejně jako volání procedury, kde ale
musíme vytvořit novou mapu proměnných z předaných argumentů.

S výhodou jsme využili \emph{curryingu} -- ač @t{evalStmt} vyžaduje tři
argumenty, na levé straně rovnice jsme uvedli pouze první dva (@t{env} a
@t{stmt}), tudíž na pravé straně musí být funkce typu @t{Turtle ->
(Turtle,DiffTrace)} (podle typové deklarace funkce @t{evalStmt}). Tímto se
zbavíme neustálého opakování a předávání argumentu @t{Turtle} jednotlivým
specializovaným funkcím.  

\subsection{Jednotlivé příkazy}

Nyní se dostáváme k implementaci jednotlivých funkcí použitých v @t{evalStmt}.

\subsubsection{Prázdná operace}

Funkci @t{noop} jsme využili v příkazu @t{IfStmt} na ošetření situace, když
podmínka neplatí, a její chování je jednoduché -- nedělá nic, takže vrátí
nezměněnou želvu a \emph{identitu}.

@Idx{Krunimir.Evaluator.noop}
\begin{code}
noop :: Turtle -> (Turtle,DiffTrace)
noop turtle = (turtle,identityDT)
\end{code}

\subsubsection{Posun vpřed}

Při pohybu vpřed musíme želvu posunout na nové místo a zkontrolovat, jestli za
sebou nezanechala čáru. Pokud ano, vrátíme @t{DiffImage}, který tuto změnu
zachycuje, pokud ne, dostaneme identitu.

@Idx{Krunimir.Evaluator.forward}
\begin{code}
forward :: Integer -> Turtle -> (Turtle,DiffTrace)
forward len turtle = (turtle',DiffTrace diff) where
  (x,y) = getPos turtle
  ang   = getAngle turtle
  p     = getPen turtle
  x'    = x + sinDeg ang * fromIntegral len
  y'    = y - cosDeg ang * fromIntegral len
  turtle' = turtle { getPos = (x',y') } 
  segment = Segment (x,y) (x',y') (getColor turtle) p
  diff = if p > 0 then SegmentTrace segment else id
\end{code}

Funkce @t{sinDeg} a @t{cosDeg}, které počítají sinus a kosinus úhlu ve stupních,
si definujeme později.

\subsubsection{Otáčení a změny pera}

Tyto operace jednoduše změní jednotlivé vlastnosti želvy.

@Idx{Krunimir.Evaluator.rotate}
\begin{code}
rotate :: Integer -> Turtle -> (Turtle,DiffTrace)
rotate ang turtle = (turtle',identityDT) where
  turtle' = turtle { getAngle = getAngle turtle + ang }
\end{code}

@Idx{Krunimir.Evaluator.pen}
\begin{code}
pen :: Integer -> Turtle -> (Turtle,DiffTrace)
pen p turtle = (turtle',identityDT) where
  turtle' = turtle { getPen = fromIntegral p }
\end{code}

Při změně barvy musíme dbát na to, ať se nějaká ze složek RGB modelu nedostane
mimo povolený rozsah 0 až 255.

@Idx{Krunimir.Evaluator.color}
\begin{code}
color :: Integer -> Integer -> Integer -> Turtle -> (Turtle,DiffTrace)
color r g b turtle = (turtle',identityDT) where
  turtle' = turtle { getColor = (crop r,crop g,crop b) }
  crop x
    | x < 0     = 0
    | x > 255   = 255
    | otherwise = fromIntegral x
\end{code}

\subsubsection{Rozdvojení želvy}

Zbývá nám funkce @t{split}, která implementuje rozdělení želvy. Prvním
parametrem je funkce reprezentující \uv{vedlejší větev}, tedy tělo příkazu
@t{split \{ ... \}}. Této funkci předáme aktuální želvu, získáme z ní
@t{DiffTrace}, který následně aplikujeme na @t{EmptyTrace}, čímž získáme hodnotu
@t{Trace} reprezentující stopu, kterou \uv{naklonovaná} želva za sebou
zanechala. Vrátíme stav původní želvy a ve výsledné hodnotě @t{DiffTrace}
uložíme částečně aplikovaný konstruktor @t{SplitTrace}.

@Idx{Krunimir.Evaluator.split}
\begin{code}
split :: (Turtle -> (Turtle,DiffTrace)) -> Turtle -> (Turtle,DiffTrace)
split f turtle = 
  let (_,dt) = f turtle
      branch = applyDT dt EmptyTrace
  in (turtle,DiffTrace { applyDT = SplitTrace branch })
\end{code}

\subsection{Vyhodnocení výrazů}

Typ funkce @t{evalExpr} jsme si představili již dříve, její implementace je
přímočará:

@Idx{Krunimir.Evaluator.evalExpr}
\begin{code}
evalExpr :: Env -> Expr -> Integer
evalExpr _   (LiteralExpr n) = n
evalExpr env (VariableExpr name) = lookupVar env name
evalExpr env (BinopExpr op left right) =
  let a = evalExpr env left
      b = evalExpr env right
  in case op of
    AddOp -> a + b
    SubOp -> a - b
    MulOp -> a * b
    DivOp -> a `div` b
evalExpr env (NegateExpr expr) = negate $ evalExpr env expr
\end{code}

\subsection{Pomocné funkce}

Zbývá nám definovat jen pomocné funkce pro vyhledávání proměnných a procedur v
@t{Env}:

@Idx{Krunimir.Evaluator.lookupDef}
@Idx{Krunimir.Evaluator.lookupVar}
\begin{code}
lookupDef :: Env -> String -> Define
lookupDef (Env procmap _) name =
  case M.lookup name procmap of
    Just def -> def
    Nothing -> error $ "Undefined procedure " ++ name

lookupVar :: Env -> String -> Integer
lookupVar (Env _ varmap) name =
  case M.lookup name varmap of
    Just num -> num
    Nothing -> error $ "Undefined variable " ++ name
\end{code}

Funkce @t{makeEnv} vytvoří nový @t{Env} s hodnotami proměnných z
\emph{asociativního seznamu} @t{binds}:

@Idx{Krunimir.Evaluator.makeEnv}
\begin{code}
makeEnv :: Env -> [(String,Integer)] -> Env
makeEnv (Env procmap _) binds = Env procmap $ M.fromList binds
\end{code}

A nakonec funkce sinus a kosinus na stupních:

@Idx{Krunimir.Evaluator.sinDeg}
@Idx{Krunimir.Evaluator.cosDeg}
\begin{code}
sinDeg, cosDeg :: Integer -> Float
sinDeg n = sin $ fromIntegral n * pi / 180.0
cosDeg n = cos $ fromIntegral n * pi / 180.0
\end{code}
