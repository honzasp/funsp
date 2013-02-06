\begin{code}
module Krunimir.Evaluator (eval) where
import Krunimir.Trace
import Krunimir.Ast

import qualified Data.Map as M
import Data.List (genericReplicate)

data Turtle = Turtle
  { getPos :: (Float,Float)
  , getAngle :: Integer
  , getColor :: (Int,Int,Int)
  , getPen :: Int
  }

data Env = Env FunMap VarMap
type FunMap = M.Map String Define
type VarMap = M.Map String Integer

newtype DiffTrace = DiffTrace (Trace -> Trace)

eval :: Program -> Trace
eval prog = diff EmptyTrace
  where
  (_,DiffTrace diff) = evalStmts env stmts startTurtle

  (defs,stmts) = foldl go ([],[]) (reverse prog)
    where go (ds,ss) topstmt = case topstmt of
            TopDefine def -> (def:ds,ss)
            TopStmt stmt -> (ds,stmt:ss)

  env = Env funMap varMap
  funMap = M.fromList [(defineName def,def) | def <- defs]
  varMap = M.empty

  startTurtle = Turtle
    { getPos = (351,351)
    , getAngle = 0
    , getColor = (0,0,0)
    , getPen = 0
    }

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
lookupDef (Env funmap _) name =
  case M.lookup name funmap of
    Just def -> def
    Nothing -> error $ "Undefined function " ++ name

lookupVar :: Env -> String -> Integer
lookupVar (Env _ varmap) name =
  case M.lookup name varmap of
    Just num -> num
    Nothing -> error $ "Undefined variable " ++ name

makeEnv :: Env -> [(String,Integer)] -> Env
makeEnv (Env funmap _) binds = Env funmap $ M.fromList binds

sinDeg, cosDeg :: Integer -> Float
sinDeg n = sin $ fromIntegral n * pi / 180.0
cosDeg n = cos $ fromIntegral n * pi / 180.0

identityDT :: DiffTrace
identityDT = DiffTrace id
\end{code}
