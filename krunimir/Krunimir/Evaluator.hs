module Krunimir.Evaluator (eval) where
import Krunimir.Image
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

newtype DiffImage = DiffImage (Image -> Image)

eval :: Program -> Image
eval prog = imageFun EmptyImg
  where
  (_,DiffImage imageFun) = evalStmts env stmts startTurtle

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

evalStmts :: Env -> [Stmt] -> Turtle -> (Turtle,DiffImage)
evalStmts _ [] turtle = (turtle,identityDI)
evalStmts env (stmt:stmts) turtle = 
  let (turtle',DiffImage imageFun) = evalStmt env stmt turtle
      (turtle'',DiffImage imageFun') = evalStmts env stmts turtle'
  in (turtle'',DiffImage $ imageFun . imageFun')

evalStmt :: Env -> Stmt -> Turtle -> (Turtle,DiffImage)
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

noop :: Turtle -> (Turtle,DiffImage)
noop turtle = (turtle,identityDI)

forward :: Integer -> Turtle -> (Turtle,DiffImage)
forward len turtle = (turtle',DiffImage imageFun) where
  (x,y) = getPos turtle
  ang = getAngle turtle
  p = getPen turtle
  x' = x + sinDeg ang * fromIntegral len
  y' = y - cosDeg ang * fromIntegral len
  turtle' = turtle { getPos = (x',y') }
  segment = Segment (round x,round y) (round x',round y') (getColor turtle) p
  imageFun = if p > 0 then SegmentImg segment else id

rotate :: Integer -> Turtle -> (Turtle,DiffImage)
rotate ang turtle = (turtle',identityDI) where
  turtle' = turtle { getAngle = getAngle turtle + ang }

pen :: Integer -> Turtle -> (Turtle,DiffImage)
pen p turtle = (turtle',identityDI) where
  turtle' = turtle { getPen = fromIntegral p }

color :: Integer -> Integer -> Integer -> Turtle -> (Turtle,DiffImage)
color r g b turtle = (turtle',identityDI) where
  turtle' = turtle { getColor = (crop r,crop g,crop b) }
  crop x
    | x < 0     = 0
    | x > 255   = 255
    | otherwise = fromIntegral x

split :: (Turtle -> (Turtle,DiffImage)) -> Turtle -> (Turtle,DiffImage)
split f turtle = 
  let (_,DiffImage imageFun) = f turtle
  in (turtle,DiffImage $ SplitImg (imageFun EmptyImg))

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

identityDI :: DiffImage
identityDI = DiffImage id
