\section{\texorpdfstring{@t{Banshee.Main}}{Banshee.Main}}
@Idx{Banshee.Main}

\begin{code}
module Banshee.Main(main) where
import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import Control.Applicative
import Control.Monad
import Data.Array

import Banshee.Castle
import Banshee.CastleParser
import Banshee.Navigate
import Banshee.Interactive 
\end{code}

@Idx{Banshee.Main.Flag}
\begin{code}
data Flag
  = HelpFlag
  | NotThroughFlag
  | QuietFlag
  | InteractiveFlag
  | JsonFlag
  deriving (Eq,Show)
\end{code}

@Idx{Banshee.Main.options}
\begin{code}
options =
  [ Option ['h','?'] ["help","use"] (NoArg HelpFlag) 
    "show the help"
  , Option ['n'] ["not-through"] (NoArg NotThroughFlag)
    "disable going through the walls"
  , Option ['q'] ["quiet"] (NoArg QuietFlag)
    "show just the minimal information about the found path"
  , Option ['i'] ["interactive"] (NoArg InteractiveFlag)
    "display a minimal interactive UI to show the found path"
  , Option ['j'] ["json"] (NoArg JsonFlag)
    "show the result as JSON"
  ]
\end{code}

\begin{code}
header progname = "Usage: " ++ progname ++ " [-hnq?] castle-file"
\end{code}

@Idx{Banshee.Main.main}
\begin{code}
main = do
  progname <- getProgName
  let usage = usageInfo (header progname) options
  (flags,files,errs) <- getOpt Permute options <$> getArgs
\end{code}

\begin{code}
  if not (null errs) then do
      hPutStrLn stderr usage
      hPutStrLn stderr $ concat errs
      exitFailure
    else return ()

  let thruWalls = NotThroughFlag `notElem` flags
      quiet = QuietFlag `elem` flags
      help = HelpFlag `elem` flags
      interactive = InteractiveFlag `elem` flags
      json = JsonFlag `elem` flags
\end{code}

\begin{code}
  if help then do
      putStrLn usage
      exitSuccess
    else return ()
\end{code}

\begin{code}
  if length files /= 1 then do
      hPutStrLn stderr usage
      hPutStrLn stderr "Expected one input file with castle"
      exitFailure
    else return ()
\end{code}

\begin{code}
  txt <- readFile (head files)
  castle <- case parseCastle (head files) txt of
      Right c -> return c
      Left err -> do
        hPutStrLn stderr (show err)
        exitFailure

  let slices = sliceCastle castle
      ui | quiet       = showQuiet castle
         | interactive = showInteractive castle slices
         | json        = showJson castle
         | otherwise   = showPath castle

  ui $ navigate castle slices thruWalls
        
  return ()
\end{code}

@Idx{Banshee.Main.showQuiet}
\begin{code}
showQuiet :: Castle -> Maybe [Loc] -> IO ()
showQuiet _ Nothing = 
  putStrLn "No path found"
showQuiet castle (Just locs) = 
  putStrLn . concat $ ["Found a path with ",show $ length locs," steps",
    " (",show $ countWalls castle locs," through walls)"]
\end{code}

@Idx{Banshee.Main.showPath}
\begin{code}
showPath :: Castle -> Maybe [Loc] -> IO ()
showPath _ Nothing = 
  putStrLn "No path found"
showPath castle (Just locs) = do
  forM_ [1..height] $ \y -> do
    forM_ [1..width] $ \x -> do
      putChar $ ary ! (x,y)
      putChar ' '
    putChar '\n'
  putStrLn . concat $ [show $ length locs," steps, ",
    " (",show $ countWalls castle locs," through walls)"]
  where

  ((1,1),(width,height)) = bounds $ castleFields castle
  fieldChar Free = '.'
  fieldChar Wall = 'X'

  fieldAry = fieldChar `fmap` castleFields castle
  ary = fieldAry // [((x,y),pathChar (x,y)) | (x,y) <- locs]

  pathChar (x,y) = case castleFields castle ! (x,y) of
      Free -> '+'
      Wall -> '~'
\end{code}

@Idx{Banshee.Main.countWalls}
\begin{code}
countWalls :: Castle -> [Loc] -> Int
countWalls castle locs =
  length $ filter ((==Wall) . (castleFields castle !)) locs
\end{code}

@Idx{Banshee.Main.showJson}
\begin{code}
showJson :: Castle -> Maybe [Loc] -> IO ()
showJson _ Nothing =
  putStrLn "{}"
showJson castle (Just locs) = 
  putStrLn . concat $ ["{ \"steps\": ",show $ length locs,
    ", \"walls\": ",show $ countWalls castle locs," }"]
\end{code}
