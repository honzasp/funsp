\section{\texorpdfstring{@t{Krunimir.Ast}}{Krunimir.Ast}}
@Idx{Krunimir.Ast}

V tomto modulu definujeme datové typy popisující syntaktický strom, které
využijeme v modulech @t{Krunimir.Parser} a @t{Krunimir.Evaluator}.

\subsection{Definice typů}

\begin{code}
module Krunimir.Ast where
\end{code}

\subsubsection{Příkazy}

Příkaz je reprezentován datovým typem @t{Stmt}, který obsahuje konstruktor
pro každý z příkazů želvího jazyka.

@Idx{Krunimir.Ast.Stmt}
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

@Idx{Krunimir.Ast.Expr}
@Idx{Krunimir.Ast.Op}
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

@Idx{Krunimir.Ast.Define}
@Idx{Krunimir.Ast.defineName}
@Idx{Krunimir.Ast.defineParams}
@Idx{Krunimir.Ast.defineStmts}
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

@Idx{Krunimir.Ast.Program}
@Idx{Krunimir.Ast.TopStmt}
\begin{code}
type Program = [TopStmt]

data TopStmt = TopDefine Define | TopStmt Stmt
  deriving Show

\end{code}

\subsection{Příklad}

Využijeme program, který nakreslí binární strom a který jsme již jednou uvedli
(jeho výstup je na obrázku \ref{fig:krunimir-bintree}). Pro přehlednost ještě
jednou zopakujeme jeho kód:

\lstinputlisting[style=krunimir]{krunimir/examples/bintree.txt}

Tento program je reprezentován pomocí výše uvedených typů následovně (text
následující za @t{-}@t{-} jsou komentáře):

% -- se musí rozdělit na - a -, protože dohromady se "spojí" na pomlčku

\begin{haskell}
[
  -- nejprve definice procedury `tree'
  TopDefine (Define {
    defineName = "tree", -- jméno definované procedury
    defineParams = ["n"], -- seznam parametrů
    defineStmts = [ -- seznam příkazů v těle procedury
      IfStmt (VariableExpr "n") [
        -- příkaz `forward(40+20*n)'
        ForwardStmt (BinopExpr AddOp
          (LiteralExpr 40)
          -- podvýraz `20*n'
          (BinopExpr MulOp (LiteralExpr 20) (VariableExpr "n"))),
        -- příkaz `split \{ left(5+3*n) tree(n-1) \}'
        SplitStmt [
          LeftStmt (BinopExpr AddOp
            (LiteralExpr 5)
            (BinopExpr MulOp (LiteralExpr 3) (VariableExpr "n"))),
          CallStmt "tree" [BinopExpr SubOp (VariableExpr "n") (LiteralExpr 1)]
        ],
        -- příkaz `right(5+3*n)'
        RightStmt (BinopExpr AddOp 
          (LiteralExpr 5) 
          (BinopExpr MulOp (LiteralExpr 3) (VariableExpr "n"))),
        -- příkaz `tree(n-1)'
        CallStmt "tree" [BinopExpr SubOp (VariableExpr "n") (LiteralExpr 1)]
      ]
    ]
  }),
  -- trojice příkazů následujících za definicí procedury
  TopStmt (ForwardStmt (NegateExpr (LiteralExpr 250))),
  TopStmt (PenStmt (LiteralExpr 1)),
  TopStmt (CallStmt "tree" [LiteralExpr 5])
]
\end{haskell}
