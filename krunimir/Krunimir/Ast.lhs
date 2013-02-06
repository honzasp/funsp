\section{@t{Krunimir.Ast}}

V tomto modulu definujeme datové typy popisující syntaktický strom, které
využijeme v modulech @t{Krunimir.Parser} a @t{Krunimir.Evaluator}.

\subsection{Definice typů}

\begin{code}
module Krunimir.Ast where
\end{code}

\subsubsection{Příkazy}

Příkaz je reprezentován datovým typem @t{Stmt}, který obsahuje konstruktor
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

\subsubsection{Výrazy}

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

\subsubsection{Definice}

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

\subsubsection{Programy}

Na nejvyšší úrovni v programu se mohou nacházet jak definice funkcí, tak
příkazy, což odráží typ @t{TopStmt}. Těmto strukturám budeme říkat
\emph{top-příkazy}. Želví program je pak jen seznam těchto \uv{top-příkazů}.

\begin{code}
type Program = [TopStmt]

data TopStmt = TopDefine Define | TopStmt Stmt
  deriving Show

\end{code}

\subsection{Příklady}

\marginnote{Sem se dá jeden z dříve uvedených příkladů a jeho reprezentace v
AST.}

% TODO
