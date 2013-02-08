\section{@t{Krunimir.PngRenderer}}
@Idx{Krunimir.PngRenderer}

K renderování stop ve formátu PNG použijeme knihovnu \emph{GD}. Její výhodou je,
že je velmi jednoduchá na použití. Bohužel neumožňuje vykreslovat čáry jiné
tloušťky než 1~px, takže informaci o tloušťce pera nemůžeme využít.

\begin{code}
module Krunimir.PngRenderer(renderPng) where
import Krunimir.Trace
import qualified Graphics.GD as GD
\end{code}

Veškeré operace s obrázky jsou v knihovně GD implementovány jako operace v
monádě IO.

@Idx{Krunimir.PngRenderer.renderPng}
\begin{code}
renderPng :: Trace -> FilePath -> IO ()
renderPng trace fpath = do
  gimg <- GD.newImage (701,701)
  GD.fillImage (GD.rgb 255 255 255) gimg
  mapM_ (mapM_ $ drawSegment gimg) (traceToSegss trace)
  GD.savePngFile fpath gimg
  where
  drawSegment :: GD.Image -> Segment -> IO ()
  drawSegment gimg (Segment (x1,y1) (x2,y2) (r,g,b) _pen) =
    GD.drawLine (floor x1,floor y1) (floor x2,floor y2) (GD.rgb r g b) gimg
\end{code}
