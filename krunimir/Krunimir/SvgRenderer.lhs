\section{@t{Krunimir.SvgRenderer}}
@Idx{Krunimir.SvgRenderer}

Na exportování do SVG nebudeme potřebovat žádnou speciální knihovnu,
jelikož se jedná o formát založený na XML.

\begin{code}
module Krunimir.SvgRenderer(renderSvg) where
import Krunimir.Trace
\end{code}

Podobně jako v modulu @t{Krunimir.PngRenderer} si nejprve stopu převedeme funkcí 
@t{Krunimir.Trace.traceToSegss} na seznam seznamů segmentů. Každému segmentu
poté pouze vytvoříme jeden element @t{<line/>}, kterým se v SVG reprezentuje
jednoduchá úsečka.

Tyto elementy stačí obalit do kořenového elementu @t{<svg> ... </svg>}, ve
kterém specifikujeme velikost obrázku, a máme hotovo!

@Idx{Krunimir.SvgRenderer.renderSvg}
\begin{code}
renderSvg :: Trace -> FilePath -> IO ()
renderSvg trace fpath =
  writeFile fpath $
       svgHeader
    ++ (unlines . map svgSegment . concat . traceToSegss $ trace)
    ++ svgFooter
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
\end{code}
