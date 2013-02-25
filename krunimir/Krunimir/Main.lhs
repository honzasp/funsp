\section{\texorpdfstring{@t{Krunimir.Main}}{Krunimir.Main}}
@Idx{Krunimir.Main}

\begin{code}
module Krunimir.Main (main) where
\end{code}

Potřebujeme několik funkcí z knihoven

\begin{code}
import System.Environment (getArgs,getProgName)
import System.IO (stderr,hPutStrLn)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)
import Data.Time.Clock (getCurrentTime,diffUTCTime)
\end{code}

a také námi definované funkce z ostatních modulů

\begin{code}
import Krunimir.Parser(parse)
import Krunimir.Evaluator(eval)
import Krunimir.PngRenderer(renderPng)
import Krunimir.SvgRenderer(renderSvg)
import Krunimir.Trace(prune)

main :: IO ()
main = do
\end{code}
@Idx{Krunimir.Main.main}

Uložíme si čas na začátku, bude se nám hodit až budeme chtít zjistit, jak dlouho
výpočet trval.

\begin{code}
  startTime <- getCurrentTime
\end{code}

Nejprve se podíváme, jaké argumenty jsme dostali na příkazové řádce, a podle
toho nastavíme proměnnou @t{inputFile} obsahující jméno vstupního souboru,
a @t{steps}, což je @t{Just \textit{početKroků}} pokud máme zadaný
počet kroků, nebo @t{Nothing} pokud jej zadaný nemáme (takže předpokládáme
že uživatel chce vykreslit celý obrázek).\footnote{V zadání je specifikováno, že
nula zadaná jako počet kroků znamená vykreslit celý obrázek, a chování našeho
programu je odlišné -- nevykreslí nic.}

\begin{code}
  args <- getArgs
  (inputFile,steps) <- case args of
    [file] -> return (file,Nothing)
    [file,steps] -> return (file,Just $ read steps)
    _ -> do
      progname <- getProgName
      hPutStrLn stderr $ "Use: " ++ progname ++ " input-file [steps]"
      exitFailure
\end{code}

@t{exitFailure} je speciální IO operace, která způsobí že program skončí s
návratovým kódem, který signalizuje selhání.

Nyní můžeme přečíst požadovaný soubor a jeho obsah předat funkci @t{parse}
z modulu @t{Krunimir.Parser}. Pokud dostaneme chybu, zobrazíme ji na
chybový výstup a program přerušíme.

@idx{Krunimir.Parser.parse}
\begin{code}
  txt <- readFile inputFile
  ast <- case parse inputFile txt of
    Right ast -> return ast
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
\end{code}

Úspěšně přečtený syntaktický strom můžeme předat funkci @t{eval} a
dostaneme výslednou stopu v písku (@t{fullTrace}). Pokud uživatel zadal omezení
počtu kroků, pomocí funkce @t{prune} stopu omezíme, pokud ne, necháme ji
celou (@t{prunedTrace}).

@idx{Krunimir.Evaluator.eval}
@idx{Krunimir.Trace.prune}
\begin{code}
  let fullTrace = eval ast
      prunedTrace = case steps of
        Nothing -> fullTrace 
        Just count -> prune count fullTrace
\end{code}

Všimněte si, že díky línému vyhodnocování se v případě, kdy je počet kroků
omezen, vypočítá jen část stopy, která nás zajímá. Máme tedy zajištěno, že i
když bude mít stopa desetitisíce kroků a uživatel bude chtít zobrazit jen
prvních několik, nebudeme počítat celou stopu, ale jen zobrazenou část. Naše
implementace dokonce umožňuje spouštět nekonečné programy, samozřejmě pouze
pokud uživatel specifikuje počet kroků, jež si přeje vykonat.

\begin{code}
  let outputPng = replaceExtension inputFile ".test.png"
      outputSvg = replaceExtension inputFile ".test.svg"
\end{code}

Jména výstupních souborů (jak PNG, tak SVG) odvodíme ze jména souboru vstupního,
jen změníme příponu. Zbývá jen vykreslit

@idx{Krunimir.PngRenderer.renderPng}
@idx{Krunimir.SvgRenderer.renderSvg}
\begin{code}
  renderPng prunedTrace outputPng
  renderSvg prunedTrace outputSvg
\end{code}

a vypsat řádek, který nás informuje o délce výpočtu.

\begin{code}
  endTime <- getCurrentTime
  putStrLn $ show (diffUTCTime endTime startTime) ++ " : " ++ inputFile
\end{code}
