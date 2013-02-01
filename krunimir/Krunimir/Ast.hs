module Krunimir.Ast where

type Program = [TopStmt]

data TopStmt = TopDefine Define | TopStmt Stmt
  deriving Show

data Stmt = 
    ForwardStmt Expr
  | LeftStmt Expr
  | RightStmt Expr
  | PenStmt Expr
  | ColorStmt Expr Expr Expr
  | RepeatStmt Expr [Stmt]
  | IfStmt Expr [Stmt]
  | SplitStmt [Stmt]
  | FunStmt String [Expr]
  deriving Show

data Expr =
    Literal Integer
  | Variable String
  | Binop Op Expr Expr
  deriving Show

data Op = AddOp | SubOp | MulOp | DivOp
  deriving Show

data Define = Define
  { defineName :: String
  , defineParams :: [String]
  , defineStmts :: [Stmt]
  } deriving Show

