module Krunimir.Evaluator (eval) where
import Krunimir.Image
import Krunimir.Ast

import qualified Data.Map as M
import Data.List (genericReplicate)

data Turtle = Turtle
  { getPos :: (Double,Double)
  , getAngle :: Integer
  , getColor :: (Int,Int,Int)
  , getPen :: Int
  }

data Env = Env FunMap VarMap
type FunMap = M.Map String Define
type VarMap = M.Map String Integer

type TI = Turtle -> Image -> (Turtle,Image)

eval :: Program -> Image
eval prog = snd $ evalStmts env stmts startTurtle EmptyImg
  where
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

evalStmts :: Env -> [Stmt] -> TI
evalStmts _ [] = noop
evalStmts env (stmt:stmts) = \turtle image ->
  let (turtle'',image') = evalStmts env stmts turtle' image
      (turtle',image'') = evalStmt env stmt turtle image'
  in (turtle'',image'')

evalStmt :: Env -> Stmt -> TI
evalStmt env stmt = case stmt of
  ForwardStmt e -> forward (ee e)
  LeftStmt e    -> rotate (negate $ ee e)
  RightStmt e   -> rotate (ee e)
  PenStmt e     -> pen (ee e)
  ColorStmt r g b -> color (ee r) (ee g) (ee b)
  RepeatStmt e stmts -> evalStmts env $ concat $ genericReplicate (ee e) stmts
  IfStmt e stmts -> if ee e > 0 then evalStmts env stmts else noop
  SplitStmt stmts -> split $ evalStmts env stmts
  FunStmt name args -> let
      def = lookupDef env name
      binds = zip (defineParams def) (map ee args)
      newenv = makeEnv env binds
    in evalStmts newenv (defineStmts def)
  where ee = evalExpr env

noop :: TI
noop turtle image = (turtle,image)

forward :: Integer -> TI
forward len turtle image = (turtle',image') where
  (x,y) = getPos turtle
  ang = getAngle turtle
  p = getPen turtle
  x' = x + sinDeg ang * fromIntegral len
  y' = y - cosDeg ang * fromIntegral len
  turtle' = turtle { getPos = (x',y') }
  segment = Segment (round x,round y) (round x',round y') (getColor turtle) p
  image' = if p > 0 then SegmentImg segment image else image

rotate :: Integer -> TI
rotate ang turtle image = (turtle',image) where
  turtle' = turtle { getAngle = getAngle turtle + ang }

pen :: Integer -> TI
pen p turtle image = (turtle',image) where
  turtle' = turtle { getPen = fromIntegral p }

color :: Integer -> Integer -> Integer -> TI
color r g b turtle image = (turtle',image) where
  turtle' = turtle { getColor = (crop r,crop g,crop b) }
  crop x
    | x < 0     = 0
    | x > 255   = 255
    | otherwise = fromIntegral x

split :: TI -> TI
split lti tur img =
  let (_,limg) = lti tur EmptyImg
  in (tur,SplitImg limg img)

evalExpr :: Env -> Expr -> Integer
evalExpr _ (Literal n) = n
evalExpr env (Variable name) = lookupVar env name
evalExpr env (Binop op left right) =
  let a = evalExpr env left
      b = evalExpr env right
  in case op of
    AddOp -> a + b
    SubOp -> a - b
    MulOp -> a * b
    DivOp -> a `div` b

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

sinDeg, cosDeg :: Integer -> Double
sinDeg n = sin $ fromIntegral n * pi / 180.0
cosDeg n = cos $ fromIntegral n * pi / 180.0
