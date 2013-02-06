\section{@t{Krunimir.Main}}

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
import Krunimir.Renderer(render)
import Krunimir.Trace(prune)

main :: IO ()
main = do
\end{code}

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
programu je odlišné - nevykreslí nic.}

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

Jméno výstupního souboru necháme stejné jako vstupního, jen změníme příponu.

\begin{code}
  let fullTrace = eval ast
      prunedTrace = case steps of
        Nothing -> fullTrace 
        Just count -> prune count fullTrace
      outputFile = replaceExtension inputFile ".test.png"
\end{code}

Zbývá jen vykreslit

\begin{code}
  render prunedTrace outputFile
\end{code}

a vypsat řádek, který nás informuje o délce výpočtu.

\begin{code}
  endTime <- getCurrentTime
  putStrLn $ show (diffUTCTime endTime startTime) ++ " : " ++ inputFile
\end{code}
