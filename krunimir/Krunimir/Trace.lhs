\section{\texorpdfstring{@t{Krunimir.Trace}}{Krunimir.Trace}}
@Idx{Krunimir.Trace}

Než představíme vyhodnocování programu reprezentovaného syntaktickým stromem,
musíme ukázat modul @t{Krunimir.Trace}, který poskytuje datové typy pro práci se
stopami složenými z čar, které za sebou zanechává želva pochodující po písku.
Tyto stopy jsou výstupem funkce @t{Krunimir.Evaluator.eval}.

\begin{code}
module Krunimir.Trace
( Trace(..)
, Segment(..)
, prune
, traceToSegss
) where
\end{code}

\subsection{Typy}
@Idx{Krunimir.Trace.Trace}

Nejdůležitějším typem je @t{Trace}, reprezentující stopu želvy. @t{Trace} má tři
konstruktory:

\begin{description}
\item[@t{EmptyTrace}] je prázdná stopa, tedy nic.
\item[@t{SplitTrace}] reprezentuje rozdělení stopy na dvě v místě příkazu
  @t{split}.
\item[@t{SegmentTrace}] je stopa tvořená úsečkou, za kterou následuje další
  stopa.
\end{description}

\begin{code}
data Trace
  = EmptyTrace
  | SplitTrace Trace Trace
  | SegmentTrace Segment Trace
  deriving Show
\end{code}

@Idx{Krunimir.Trace.Segment}
Typ @t{Segment} představuje úsečku mezi dvěma body, která má barvu a tloušťku.

\begin{code}
data Segment = Segment Point Point Color Int 
  deriving Show

type Point = (Float,Float)
type Color = (Int,Int,Int)
\end{code}

\input{tex/trace-example.tex}

\subsection{Funkce}

V ostatních modulech budeme potřebovat pomocné funkce @t{prune} a
@t{traceToSegss}.

\subsubsection{Funkce \texorpdfstring{@t{prune}}{prune}}
@Idx{Krunimir.Trace.prune}

Funkce @t{prune} omezí počet \emph{tahů}, které stopa zahrnuje.

\begin{code}
prune :: Integer -> Trace -> Trace
prune n img
  | n <= 0 = EmptyTrace
  | otherwise = case img of
    EmptyTrace -> EmptyTrace
    SplitTrace l r -> SplitTrace (prune n l) (prune n r)
    SegmentTrace seg x -> SegmentTrace seg $ prune (n-1) x
\end{code}

\subsubsection{Funkce \texorpdfstring{@t{traceToSegss}}{traceToSegss}}
@Idx{Krunimir.Trace.traceToSegss}

Funkce @t{traceToSegss} transformuje stopu do seznamu seznamů segmentů.
\uv{Vnější} seznam reprezentuje jednotlivé segmenty jednoho tahu. Tato funkce se
využívá při vykreslování a seřazení jednotlivých segmentů podle tahů zajišťuje
korektní překrytí čar.\footnote{Kdybychom se rozhodli vzájemné překrytí čar
zanedbat, celý program by byl \emph{výrazně} jednodušší.}

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

V případě @t{SplitTrace} nejprve vyhodnotíme seznamy seznamů segmentů pro
každou stranu zvlášt a pak je pomocnou funkcí @t{zipSegs} \uv{slijeme}
dohromady.
