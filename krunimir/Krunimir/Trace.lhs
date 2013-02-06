\section{\texttt{Krunimir.Trace}}

Modul @t{Krunimir.Trace} poskytuje datové typy pro práci se stopami složenými z
čar, které za sebou zanechává želva pochodující po písku.

\begin{code}
module Krunimir.Trace
( Trace(..)
, Segment(..)
, prune
, traceToSegss
) where
\end{code}

\subsection{Typy}

Nejdůležitějším typem je @t{Trace}, reprezentující stopu želvy. @t{Trace} má tři
konstruktory:

\begin{description}
\item[@t{EmptyTrace}] je prázdná stopa, tedy nic.
\item[@t{SplitTrace}] reprezentuje dva obrázky, které vzniknou jako důsledek
  \uv{rozdělení} želvy příkazem @t{split}.
\item[@t{SegmentTrace}] je obrázek tvořený jedním @t{Segment}em (úsečkou) a dalším
  obrázkem.
\end{description}

\begin{code}
data Trace
  = EmptyTrace
  | SplitTrace Trace Trace
  | SegmentTrace Segment Trace
  deriving Show
\end{code}

Typ @t{Segment} představuje úsečku mezi dvěma body, která má barvu a tloušťku.

\begin{code}
data Segment = Segment Point Point Color Int 
  deriving Show

type Point = (Int,Int)
type Color = (Int,Int,Int)
\end{code}

\marginnote{Opět by se hodil drobný diagram, reprezentující nějaký jednoduchý
obrázek.}

\subsection{Funkce}

V ostatních modulech budeme rovněž potřebovat tyto pomocné funkce.

\subsubsection{Funkce @t{prune}}

Funkce @t{prune} omezí počet kroků, které

\begin{code}
prune :: Integer -> Trace -> Trace
prune n img
  | n <= 0 = EmptyTrace
  | otherwise = case img of
    EmptyTrace -> EmptyTrace
    SplitTrace l r -> SplitTrace (prune n l) (prune n r)
    SegmentTrace seg x -> SegmentTrace seg $ prune (n-1) x
\end{code}

\subsubsection{Funkce @t{traceToSegss}}

\begin{code}
traceToSegss :: Trace -> [[Segment]]
traceToSegss EmptyTrace = []
traceToSegss (SegmentTrace seg img) = [seg]:traceToSegss img
traceToSegss (SplitTrace limg rimg) = zipSegs (traceToSegss limg) (traceToSegss rimg)
  where
  zipSegs :: [[Segment]] -> [[Segment]] -> [[Segment]]
  zipSegs [] [] = []
  zipSegs lss [] = lss
  zipSegs [] rss = rss
  zipSegs (ls:lss) (rs:rss) = (ls ++ rs):zipSegs lss rss
\end{code}
