\section{@t{Krunimir.Renderer}}
@Idx{Krunimir.Renderer}

\begin{code}
module Krunimir.Renderer(render) where
import Krunimir.Trace
import qualified Graphics.GD as GD
\end{code}

@Idx{Krunimir.Renderer.render}
\begin{code}
render :: Trace -> FilePath -> IO ()
render trace fpath = do
  gimg <- GD.newImage (701,701)
  GD.fillImage (GD.rgb 255 255 255) gimg
  mapM_ (mapM_ $ drawSegment gimg) (traceToSegss trace)
  GD.savePngFile fpath gimg
\end{code}

\begin{code}
drawSegment :: GD.Image -> Segment -> IO ()
drawSegment gimg (Segment from to (r,g,b) _pen) =
  GD.drawLine from to (GD.rgb r g b) gimg
\end{code}
