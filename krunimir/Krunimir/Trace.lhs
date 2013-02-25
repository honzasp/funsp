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

\begin{figure}

\begin{tikzpicture}[
  image/.style={x=.08mm,y=-.08mm},
  l2/.style={image,line width=.2mm},
  l4/.style={image,line width=.4mm},
  segline/.style={image,-to,shorten >=2mm,draw=black!60,thin,dashed},
  sibling distance=5cm,
  trace/.style={font=\ttfamily},
  segtr/.style={trace},
  split/.style={trace},
  empty/.style={trace},
  seg/.style={font=\ttfamily\footnotesize,node distance=-3mm and -13mm,
    text width=35mm},
  edge from parent/.style={draw,-to},
  level distance=15mm,
]

\node[segtr] (n1) at (-6.5,0) {SegmentTrace}
  child {node[split] {SplitTrace}
    child {node[segtr] (n2) {SegmentTrace}
      child {node[segtr] (n3) {SegmentTrace}
        child { node[segtr] (n4) {SegmentTrace}
          child { node[empty] {EmptyTrace}}
        }
      }
    }
    child {node[segtr] (n5) {SegmentTrace}
      child { node[empty] {EmptyTrace} }
    }
  }
;

\node[seg,below right=of n1] (s1) {Segment (350,500) (350,350) (0,0,0) 2} ;
\node[seg,below right=of n2] (s2) {Segment (200,200) (200,100) (0,0,0) 2} ;
\node[seg,below right=of n3] (s3) {Segment (350,100) (500,100) (0,0,0) 4} ;
\node[seg,below right=of n4] (s4) {Segment (350,350) (600,100) (0,0,0) 4} ;
\node[seg,below right=of n5] (s5) {Segment (500,100) (600,200) (127,127,127) 4} ; 

\draw [image] (0,0) rectangle (701,701) ;
\draw [l2] (350,500) -- (350,350) ;
\draw [l2] (200,200) -- (200,100) ;
\draw [l4] (350,350) -- (600,100) ;
\draw [l4] (350,100) -- (500,100) ;
\draw [l4,color=black!50] (600,200) -- (500,100) ;

\draw [segline] (s1.south) to [bend right=10] (350,425) ;
\draw [segline] (s2.north) to [bend left=20] (200,150) ;
\draw [segline] (s3.south) .. controls +(10cm,0) .. (475,225) ;
\draw [segline] (s4.north) to [bend right=20] (425,100) ;
\draw [segline] (s5.north) to [bend left=5] (550,150) ;

\end{tikzpicture}

% there might be a cleaner solution
\begin{adjustwidth}{2cm}{0cm}
\begin{haskell}[basicstyle=\ttfamily\footnotesize]
SegmentTrace (Segment (350,500) (350,350) (0,0,0) 2)
  (SplitTrace
    (SegmentTrace (Segment (200,200) (200,100) (0,0,0) 2)
      (SegmentTrace (Segment (350,100) (500,100) (0,0,0) 4)
        (SegmentTrace (Segment (500,100) (600,200) (127,127,127) 4)
          EmptyTrace)))
    (SegmentTrace (Segment (350,350) (600,100) (0,0,0) 4)
      EmptyTrace))
\end{haskell}
\end{adjustwidth}

\caption{Grafické znázornění stopy. Obrázek nalevo odpovídá vykreslené stopě,
šipky od jednotlivých segmentů ukazují na příslušné úsečky na obrázku. Všimněte
si způsobu, jakým se čáry překrývají -- segment, jenž je blíž kořeni stromu, je
překryt segmentem nacházejícím se ve stromu \uv{hlouběji}.}
\label{fig:krunimir-trace}

\end{figure}

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
