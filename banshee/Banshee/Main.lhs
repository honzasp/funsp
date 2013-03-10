\section{\texorpdfstring{@t{Banshee.Main}}{Banshee.Main}}
@Idx{Banshee.Main}

Modul @t{Banshee.Main} obsahuje uživatelské rozhraní programu. Podobně jako u
Krunimíra budeme náš program spouštět v terminálu, ale umožníme uživateli pomocí
argumentů zadaných na příkazovém řádku měnit chování programu.

\subsection{Popis použití programu}

Programu na příkazové řádce předáme jméno vstupního souboru s hradem a navíc
můžeme předat některé z následujících přepínačů:

\begin{description}

\item[@t{-h}, @t{-?} nebo @t{-{}-help}] zobrazí návod na použití programu a
skončí.

\item[@t{-n} nebo @t{-{}-not-through}] zakáže procházení zdmi (ve výchozím
nastavení je procházení zdmi povoleno).

\item[@t{-s} nebo @t{-{}-show-castle}] vypíše předaný hrad se zvýrazněnou
nalezenou cestou (výchozí chování).

\item[@t{-q} nebo @t{-{}-quiet}] způsobí, že se vypíše pouze jeden řádek s počtem
kroků cesty a počtem zdí, kterými cesta vede (hrad ani zvýrazněná cesta se
nevypisuje).

\item[@t{-i} nebo @t{-{}-interactive}] po nalezení cesty zobrazí textové
rozhraní (založené na unixové knihovně \emph{ncurses} \cite{ncurses}), které
umožní interaktivně zobrazit průchod bílé paní hradem po nalezené nekratší
cestě, včetně pohybů zvědů.

\item[@t{-j} nebo @t{-{}-json}] zobrazí podobný výstup jako @t{-{}-quiet}, pouze
ve stojově čitelném formátu JSON \cite{crockford2006application} (využívá se při
automatickém testování programu).

\end{description}

\subsection{Zpracování argumentů z příkazové řádky}

Na zpracování argumentů předaných na příkazové řádce použijeme modul
@t{System.Console.GetOpt}, který je součástí standardní knihovny.

Začneme hlavičkou modulu:

\begin{code}
module Banshee.Main(main) where
import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import Control.Applicative
import Control.Monad
import Data.Array
import Data.List (intersect)

import Banshee.Castle
import Banshee.CastleParser (parseCastle)
import Banshee.Navigate (navigate)
import Banshee.Interactive (showInteractive)
\end{code}

\subsubsection{Definice přepínačů}

Nyní si deklarujeme jednoduchý datový typ @t{Flag}, který reprezentuje
jednotlivé přepínače, které můžeme dostat na příkazové řádce:

@Idx{Banshee.Main.Flag}
\begin{code}
data Flag
  = HelpFlag
  | NotThroughFlag
  | ShowCastleFlag
  | QuietFlag
  | InteractiveFlag
  | JsonFlag
  deriving (Eq,Show)
\end{code}

V seznamu @t{options} uvedeme seznam všech možných nastavení s krátkými popisy:

@Idx{Banshee.Main.options}
\begin{code}
options =
  [ Option ['h','?'] ["help"] (NoArg HelpFlag) 
    "show the help"
  , Option ['n'] ["not-through"] (NoArg NotThroughFlag)
    "disable going through the walls"
  , Option ['s'] ["show-castle"] (NoArg ShowCastleFlag)
    "show the castle with highlighted path (default)"
  , Option ['q'] ["quiet"] (NoArg QuietFlag)
    "show just the minimal information about the found path"
  , Option ['i'] ["interactive"] (NoArg InteractiveFlag)
    "display a minimal interactive UI to show the found path"
  , Option ['j'] ["json"] (NoArg JsonFlag)
    "show the result as JSON"
  ]
\end{code}

Funkce @t{header} vytvoří hlavičku návodu na použití. Její argument @t{progname}
je jméno programu (ve většině případů to bude @t{"banshee"}).

\begin{code}
header progname = "Usage: " ++ progname ++ " [flags] castle-file"
\end{code}

\subsubsection{Čtení předaných přepínačů}

Na začátku @t{main} získáme seznam předaných argumentů z příkazové řádky pomocí
@t{getArgs} a předáme jej funkci @t{getOpt} z knihovny @t{GetOpt}. První
argument této funkce je hodnota @t{Permute}, která značí, že přepínače a jména
souborů mohou být na příkazové řádce uvedeny v libovolném pořadí, druhým
argumentem je seznam @t{options}, třetím je výsledek funkce @t{getArgs}.

@Idx{Banshee.Main.main}
\begin{code}
main :: IO ()
main = do
  (flags,files,errs) <- getOpt Permute options <$> getArgs
\end{code}

Výsledkem funkce @t{getOpt} je trojice:

\begin{enumerate}
\item @t{flags} je seznam předaných přepínačů (@t{[Flag]}).
\item @t{files} je seznam předaných jmen souborů (@t{[String]}).
\item Pokud uživatel udělal chybu při zadávání přepínačů, v seznamu @t{errs}
budou chybové hlášky (@t{[String]}).
\end{enumerate}

\subsubsection{Kontrola přepínačů}

Ještě než se začneme zabývat předanými přepínači, vygenerujeme si návod k
použití, který posléze můžeme zobrazit uživateli v případě, že udělal chybu. K
tomu nám poslouží funkce @t{usageInfo} z knihovny @t{GetOpt}.

\begin{code}
  progname <- getProgName
  let usage = usageInfo (header progname) options
\end{code}

Nejprve zkontrolujeme, jestli seznam @t{errs} neobsahuje nějakou chybu. Pokud
ano, vypíšeme návod k použití a všechny chyby, načež program ukončíme.

\begin{code}
  if not (null errs) then do
      hPutStrLn stderr usage
      hPutStrLn stderr $ concat errs
      exitFailure
    else return ()
\end{code}

Pokud si uživatel vyžádal nápovědu, zobrazíme ji a skončíme.

\begin{code}
  if HelpFlag `elem` flags then do
      putStrLn usage
      exitSuccess
    else return ()
\end{code}

Přepínače @t{-{}-quiet}, @t{-{}-interactive}, @t{-{}-json} a @t{-{}-show-castle}
se vzájemně vylučují, vždy lze použít nejvýše jeden.

\begin{code}
  let exclusiveFlags = [QuietFlag,InteractiveFlag,JsonFlag,ShowCastleFlag]
  if (> 1) . length . intersect flags $ exclusiveFlags then do
      hPutStrLn stderr usage
      hPutStrLn stderr "The flags --quiet, --interactive, --json\
        \ and --show-castle are mutually exclusive (use at most one)"
      exitFailure
    else return ()
\end{code}

Nakonec zkontrolujeme, jestli jsme dostali právě jeden soubor; pokud ne, opět
vypíšeme návod a skončíme.

\begin{code}
  inputFile <- case files of
    [file] -> return file
    _ -> do
      hPutStrLn stderr usage
      hPutStrLn stderr "Expected one input file with castle"
      exitFailure
\end{code}

\subsection{Výpočet trasy}

Vstupní soubor přečteme a zparsujeme pomocí funkce @t{parseCastle}. Ta vrátí
@t{Right} s výsledným hradem v případě úspěchu a @t{Left} s chybou v případě
neúspěchu.

Přečtený hrad pak funkcí @t{sliceCastle} rozřežeme do @t{slices} a výsledek
hledání cesty uložíme do @t{result}. 

\begin{code}
  txt <- readFile inputFile
  castle <- case parseCastle inputFile txt of
      Right c -> return c
      Left err -> do
        hPutStrLn stderr (show err)
        exitFailure

  let slices = sliceCastle castle
      result =  navigate castle slices thruWalls
      thruWalls = NotThroughFlag `notElem` flags
\end{code}

\subsection{Zobrazení výsledku}

Do proměnné @t{ui} na základě předaných přepínačů přiřadíme příslušnou
zobrazovací funkci a vzápětí ji zavoláme s výsledkem hledání cesty, čímž končí
akce @t{main}. Jednotlivé zobrazovací funkce popíšeme v následujících
podsekcích.

\begin{code}
  let ui :: Maybe [Loc] -> IO ()
      ui | QuietFlag `elem` flags       = showQuiet castle
         | InteractiveFlag `elem` flags = showInteractive castle slices
         | JsonFlag `elem` flags        = showJson castle
         | otherwise                    = showCastle castle
  ui result
\end{code}

\subsection{Tiché zobrazení}

Funkce @t{showQuiet}, kterou použijeme u přepínače @t{-{}-quiet}, jednoduše
vypíše jeden řádek v závislosti na tom, jesli byla cesta nalezena nebo ne.

@Idx{Banshee.Main.showQuiet}
\begin{code}
showQuiet :: Castle -> Maybe [Loc] -> IO ()
showQuiet _ Nothing = 
  putStrLn "No path found"
showQuiet castle (Just locs) = 
  putStrLn . concat $ ["Found a path with ",show $ length locs," steps",
    " (",show $ countWalls castle locs," through walls)"]
\end{code}

Zde si také nadefinujeme funkci @t{countWalls}, která spočítá zdi nacházející se
na dané cestě a kterou použijeme i v dalších funkcích.

@Idx{Banshee.Main.countWalls}
\begin{code}
countWalls :: Castle -> [Loc] -> Int
countWalls castle locs =
  length $ filter ((==Wall) . (castleFields castle !)) locs
\end{code}

\subsection{Zobrazení ve formátu JSON}

Výsledek ve formátu JSON zobrazíme podobně jako ve funkci @t{showQuiet}, pouze
vypíšeme objekt ve tvaru @t{\{ "steps": \textit{počet_kroků}, "walls":
\textit{počet_zdí} \}} pokud byla cesta nalezena nebo @t{\{\}} pokud nebyla.

@Idx{Banshee.Main.showJson}
\begin{code}
showJson :: Castle -> Maybe [Loc] -> IO ()
showJson _ Nothing =
  putStrLn "{}"
showJson castle (Just locs) = 
  putStrLn . concat $ ["{ \"steps\": ",show $ length locs,
    ", \"walls\": ",show $ countWalls castle locs," }"]
\end{code}

\subsection{Zobrazení cesty v hradu}

Funkce @t{showCastle} vypíše celý hrad a vyznačí v něm nalezenou trasu,
samozřejmě pouze pokud byla nějaká cesta nalezena. Zdi jsou zaznačeny znaky
`@t{X}', volná políčka znaky `@t{.}'. Cestu značíme znaky `@t{+}', cestu skrz
zeď znakem `@t{\textasciitilde}'.

@Idx{Banshee.Main.showCastle}
\begin{code}
showCastle :: Castle -> Maybe [Loc] -> IO ()
showCastle _ Nothing = 
  putStrLn "No path found"
showCastle castle (Just locs) = do
  forM_ [1..height] $ \y -> do
    forM_ [1..width] $ \x -> do
      putChar $ ary ! (x,y)
    putChar '\n'
  putStrLn . concat $ ["\n",show $ length locs," steps",
    " (",show $ countWalls castle locs," through walls)"]
  where

  ((1,1),(width,height)) = bounds $ castleFields castle

  fieldAry = fieldChar `fmap` castleFields castle
  fieldChar Free = '.'
  fieldChar Wall = 'X'

  ary = fieldAry // [((x,y),pathChar (x,y)) | (x,y) <- locs]
  pathChar (x,y) = case castleFields castle ! (x,y) of
      Free -> '+'
      Wall -> '~'
\end{code}

Vykreslení provádíme tak, že si vytvoříme pole @t{ary} o rozměrech hradu,
zaplníme jej znaky a pak jej po řádcích vypíšeme.

Nejprve pole políček hradu, získané funkcí @t{castleFields}, převedeme pomocí
funkce @t{fmap} na pole znaků @t{fieldAry}. Následně funkcí @t{//} změníme ta
políčka, která se nachází na cestě (v seznamu pozic @t{locs}), na znak `@t{+}'
nebo `@t{\textasciitilde}' v závislosti na tom, jestli na tomto políčku je zeď
nebo ne.

\subsection{Interaktivní zobrazení}

Interaktivní zobrazení zajišťuje funkce @t{showInteractive} z modulu
@t{Banshee.Interactive}. Její typ je:

\begin{haskell}
showInteractive :: Castle -> [Slice] -> Maybe [Loc] -> IO ()
\end{haskell}

Tato funkce umožňuje krok po kroku procházet nalezenou cestu, včetně
pohybujících se zvědů. Modul @t{Banshee.Interactive} zde nebudeme přetiskovat,
jelikož je poměrně rozsáhlý (přibližně 180 řádků) a využívá knihovnu @t{ncurses}
\cite{millikin2012ncurses}, jejíž popis je mimo rozsah této práce.
