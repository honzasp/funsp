\begin{code}
module Krunimir.Renderer(render) where
import Krunimir.Image
import qualified Graphics.GD as GD

render :: Image -> FilePath -> IO ()
render img fpath = do
  gimg <- GD.newImage (701,701)
  GD.fillImage (GD.rgb 255 255 255) gimg
  mapM_ (mapM_ $ drawSegment gimg) (imageToSegss img)
  GD.savePngFile fpath gimg

drawSegment :: GD.Image -> Segment -> IO ()
drawSegment gimg (Segment from to (r,g,b) _pen) =
  GD.drawLine from to (GD.rgb r g b) gimg
\end{code}
