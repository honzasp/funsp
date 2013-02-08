-- quick and dirty!
module Krunimir.SVGRenderer(renderSVG) where
import Krunimir.Trace

renderSVG :: Trace -> FilePath -> IO ()
renderSVG trace fpath =
  writeFile fpath $
    svgHeader ++
    (unlines . map svgSegment . concat . traceToSegss $ trace) ++
    svgFooter
  where
    svgHeader = "<svg version=\"1.1\" width=\"701\" height=\"701\"\
      \ xmlns=\"http://www.w3.org/2000/svg\">\n"
    svgFooter = "</svg>\n"

    svgSegment (Segment (x1,y1) (x2,y2) (r,g,b) pen) =
      "<line x1=\"" ++ show x1 ++ "\"\
           \ y1=\"" ++ show y1 ++ "\"\
           \ x2=\"" ++ show x2 ++ "\"\
           \ y2=\"" ++ show y2 ++ "\"\
           \ stroke=\"rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++  ")\"\
           \ stroke-width=\"" ++ show pen ++ "\"/>"
