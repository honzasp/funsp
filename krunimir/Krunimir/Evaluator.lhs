\section{@t{Krunimir.Evaluator}}

Nyní se dostáváme k jádru problému, totiž samotnému \emph{vyhodnocování}
Krunimírova programu, implementováno funkcí @t{eval}. Vstupem této funkce je
syntaktický strom v podobě typu @t{Program} (což je jen synonym pro
@t{[TopStmt]}), výstupem stopa želvy jako typ @t{Trace}.

\begin{code}
module Krunimir.Evaluator (eval) where
import Krunimir.Trace
import Krunimir.Ast

import qualified Data.Map as M
import Data.List (genericReplicate)
\end{code}

Funkce @t{genericReplicate} je verze funkce @t{replicate} ze standardního
@t{Prelude}, které můžeme předat jakýkoli celočíselný typ, není omezena pouze na
@t{Int} (my jí budeme předávat @t{Integer}).

Definujeme si datový typ @t{Turtle}, který zahrnuje celý stav želvy -- její
pozici, natočení a barvu a tloušťku pera.

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
Definujeme si synonyma @t{ProcMap} a @t{VarMap}, mapující jména na definice
procedur, respektive proměnných.

\begin{code}
data Env = Env ProcMap VarMap
type ProcMap = M.Map String Define
type VarMap = M.Map String Integer
\end{code}

Nyní se musíme rozhodnout, jak přesně budeme vyhodnocování stopy v syntaktickém
stromu implementovat. Určitě bude vhodné vytvořit funkci na vyhodnocení jednoho
příkazu a jednoho výrazu.

Jaké informace potřebujeme k tomu, abychom mohli spočítat hodnotu výrazu?
Vzhledem k tomu, že se ve výrazu můžou vyskytovat argumenty aktuální procedury,
budeme potřebovat @t{Env}, z něhož získáme hodnoty aktuálních proměnných
(argumentů). Z toho nám plyne typ pro funkci @t{evalExpr}:

\begin{code}% níže je funkce definovaná i s typem, proto ho prozatím ignorujme
evalExpr :: Env -> Expr -> Integer
\end{code}

Dále bude třeba vytvořit funkci, která vyhodnotí příkaz. Parametry této funkce
bude učitě znovu @t{Env} (potřebujeme znát, jaké procedury existují) a
@t{Turtle} (musíme vědět, jaký je aktuální stav želvy). 

Jakou hodnotu bychom měli vrátit? Každý příkaz změní stav želvy, proto bychom
novou želvu měli vrátit jako část výsledku. Hlavní je ale to, jestli příkaz
nezmění stopu, kterou za sebou želva zanechává. Jakým způsobem ale tuto
\uv{změnu} reprezentovat? Nemůžeme použít přímo typ @t{Trace}, jelikož ten
reprezentuje \emph{celou} želvinu trasu, kdežto my spočteme jen její
\emph{začátek}.

Nejlepší bude, když vrátíme \emph{funkci}, která jako argument dostane @t{Trace}
získaný z \emph{následujících} příkazů a vrátí novou @t{Trace}.

Tím získáváme typ:

\begin{code}% ještě vlastně ne
evalStmt :: Env -> Stmt -> Turtle -> (Turtle,Trace -> Trace)
\end{code}

S funkcemi typu @t{Trace -> Trace} budeme pracovat často, proto si vytvoříme
\emph{nový typ}.

\begin{code}
newtype DiffTrace = DiffTrace (Trace -> Trace)
\end{code}

\marginnote{Co takhle použít field applyDI?}

S tímto novým typem, který reprezentuje \emph{rozdíl} nebo \emph{změnu} stopy
@t{Trace}, bude typ funkce @t{evalStmt} vypadat takto:

\begin{code}% už to je správné, ale pravý Haskell si necháme na později
evalStmt :: Env -> Stmt -> Turtle -> (Turtle,DiffTrace)
\end{code}

Tuto deklaraci typu můžeme chápat takto: \uv{@t{evalStmt} je funkce vyžadující
mapu procedur a proměnných uložených v typu @t{Env}, dále příkaz k vyhodnocení a
stav želvy; vrátí změněný stav želvy a \emph{změnu}, kterou tento příkaz vyvolá
na stopě želvy.}

I když toto typové kung-fu může vypadat na první pohled zbytečně komplikovaně a
složitě, opak je pravdou -- umožní nám vyhodnocování příkazů implementovat velmi
elegantně a jednoduše.

Pojďme se podívat, jak vypadá funkce @t{eval}, která vyhodnotí celý program.

\begin{code}
eval :: Program -> Trace
eval prog = 
  let (_,DiffTrace diff) = evalStmts env stmts startTurtle
  in diff EmptyTrace
  where

  (defs,stmts) = foldl go ([],[]) (reverse prog)
    where go (ds,ss) topstmt = case topstmt of
            TopDefine def -> (def:ds,ss)
            TopStmt stmt -> (ds,stmt:ss)

  env = Env procMap varMap
  procMap = M.fromList [(defineName def,def) | def <- defs]
  varMap = M.empty

  startTurtle = Turtle
    { getPos = (351,351)
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
seznam příkazů, čímž získáme @t{diff}, funkci která reprezentuje změnu, jenž
program vykoná na celkové stopě želvy. Tuto změnu aplikujeme na prázdnou stopu,
čímž získáme kýženou hodnotu @t{Trace}.

\begin{code}
evalStmts :: Env -> [Stmt] -> Turtle -> (Turtle,DiffTrace)
evalStmts _ [] turtle = (turtle,identityDT)
evalStmts env (stmt:stmts) turtle = 
  let (turtle',DiffTrace diff) = evalStmt env stmt turtle
      (turtle'',DiffTrace diff') = evalStmts env stmts turtle'
  in (turtle'',DiffTrace $ diff . diff')

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

noop :: Turtle -> (Turtle,DiffTrace)
noop turtle = (turtle,identityDT)

forward :: Integer -> Turtle -> (Turtle,DiffTrace)
forward len turtle = (turtle',DiffTrace diff) where
  (x,y) = getPos turtle
  ang = getAngle turtle
  p = getPen turtle
  x' = x + sinDeg ang * fromIntegral len
  y' = y - cosDeg ang * fromIntegral len
  turtle' = turtle { getPos = (x',y') }
  segment = Segment (round x,round y) (round x',round y') (getColor turtle) p
  diff = if p > 0 then SegmentTrace segment else id

rotate :: Integer -> Turtle -> (Turtle,DiffTrace)
rotate ang turtle = (turtle',identityDT) where
  turtle' = turtle { getAngle = getAngle turtle + ang }

pen :: Integer -> Turtle -> (Turtle,DiffTrace)
pen p turtle = (turtle',identityDT) where
  turtle' = turtle { getPen = fromIntegral p }

color :: Integer -> Integer -> Integer -> Turtle -> (Turtle,DiffTrace)
color r g b turtle = (turtle',identityDT) where
  turtle' = turtle { getColor = (crop r,crop g,crop b) }
  crop x
    | x < 0     = 0
    | x > 255   = 255
    | otherwise = fromIntegral x

split :: (Turtle -> (Turtle,DiffTrace)) -> Turtle -> (Turtle,DiffTrace)
split f turtle = 
  let (_,DiffTrace diff) = f turtle
  in (turtle,DiffTrace $ SplitTrace (diff EmptyTrace))

evalExpr :: Env -> Expr -> Integer
evalExpr _ (LiteralExpr n) = n
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

makeEnv :: Env -> [(String,Integer)] -> Env
makeEnv (Env procmap _) binds = Env procmap $ M.fromList binds

sinDeg, cosDeg :: Integer -> Float
sinDeg n = sin $ fromIntegral n * pi / 180.0
cosDeg n = cos $ fromIntegral n * pi / 180.0

identityDT :: DiffTrace
identityDT = DiffTrace id
\end{code}
