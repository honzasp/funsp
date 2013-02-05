\section{\texttt{Krunimir.Ast}}

V tomto modulu definujeme datové typy popisující syntaktický strom.

\begin{code}
module Krunimir.Ast where
\end{code}

\subsection{Příkazy}

Příkaz je reprezentován datovým typem \texttt{Stmt}, který obsahuje konstruktor
pro každý z příkazů želvího jazyka.

\begin{code}
data Stmt = 
    ForwardStmt Expr
  | LeftStmt Expr
  | RightStmt Expr
  | PenStmt Expr
  | ColorStmt Expr Expr Expr
  | RepeatStmt Expr [Stmt]
  | IfStmt Expr [Stmt]
  | SplitStmt [Stmt]
  | CallStmt String [Expr]
  deriving Show
\end{code}

\subsection{Výrazy}

Reprezentace výrazů je podobně přímočará:

\begin{code}
data Expr =
    LiteralExpr Integer
  | VariableExpr String
  | BinopExpr Op Expr Expr
  | NegateExpr Expr
  deriving Show

data Op = AddOp | SubOp | MulOp | DivOp
  deriving Show
\end{code}

\subsection{Definice}

Poslední strukturou je definice uživatelské procedury, pro kterou použijeme datový
typ s pojmenovanými prvky (záznam).

% TODO: anglický název

\begin{code}
data Define = Define
  { defineName :: String
  , defineParams :: [String]
  , defineStmts :: [Stmt]
  } deriving Show
\end{code}

\subsection{Programy}

Na nejvyšší úrovni v programu se mohou nacházet jak definice funkcí, tak
příkazy, což odráží typ \texttt{TopStmt}. Želví program je pak jen seznam těchto
\uv{top-příkazů}.

\begin{code}
type Program = [TopStmt]

data TopStmt = TopDefine Define | TopStmt Stmt
  deriving Show

\end{code}

\subsection{Příkad}

% TODO
