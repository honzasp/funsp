module Krunimir.Painter (paint) where
import Krunimir.Image
import qualified Graphics.GD as GD

paint :: Image -> FilePath -> IO ()
paint img fpath = do
  gimg <- GD.newImage (701,701)
  GD.fillImage (GD.rgb 255 255 255) gimg
  mapM_ (mapM_ $ drawSegment gimg) (imageToSegss img)
  GD.savePngFile fpath gimg

drawSegment :: GD.Image -> Segment -> IO ()
drawSegment gimg (Segment from to (r,g,b) _pen) =
  GD.drawLine from to (GD.rgb r g b) gimg

imageToSegss :: Image -> [[Segment]]
imageToSegss EmptyImg = []
imageToSegss (SegmentImg seg img) = [seg]:imageToSegss img
imageToSegss (SplitImg limg rimg) = zipSegs (imageToSegss limg) (imageToSegss rimg)
  where
  zipSegs :: [[Segment]] -> [[Segment]] -> [[Segment]]
  zipSegs [] [] = []
  zipSegs lss [] = lss
  zipSegs [] rss = rss
  zipSegs (ls:lss) (rs:rss) = (ls ++ rs):zipSegs lss rss
