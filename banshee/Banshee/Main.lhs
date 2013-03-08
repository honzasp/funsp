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

import UI.NCurses
import UI.NCurses.Panel
import Data.IORef
import Control.Monad.IO.Class

import Banshee.Castle
import Banshee.CastleParser
import Banshee.Navigate
\end{code}

@Idx{Banshee.Main.Flag}
\begin{code}
data Flag
  = HelpFlag
  | NotThroughFlag
  | QuietFlag
  | InteractiveFlag
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

  case navigate castle slices thruWalls of
    Just locs
      | quiet       -> showQuiet castle locs
      | interactive -> showInteractive castle slices locs
      | otherwise   -> showPath castle locs
    Nothing ->
        putStrLn $ "No path found"
        
  return ()
\end{code}

@Idx{Banshee.Main.showQuiet}
\begin{code}
showQuiet :: Castle -> [Loc] -> IO ()
showQuiet castle locs = 
  putStrLn . concat $ ["Found a path with ",show steps," steps",
    " (",show thruWalls," through walls)"]
  where
  steps = length locs
  fields = castleFields castle
  thruWalls = length $ filter ((==Wall) . (fields !)) locs
\end{code}

@Idx{Banshee.Main.showPath}
\begin{code}
showPath :: Castle -> [Loc] -> IO ()
showPath castle locs = do
  forM_ [1..height] $ \y -> do
    forM_ [1..width] $ \x -> do
      putChar $ ary ! (x,y)
      putChar ' '
    putChar '\n'
  showQuiet castle locs
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

\begin{code}
showInteractive :: Castle -> [Slice] -> [Loc] -> IO ()
showInteractive castle slices locs = runCurses $ do
  (rows,cols) <- screenSize
  topWin <- defaultWindow

  mapWin <- newWindow (rows-1) cols 0 0
  mapPan <- newPanel mapWin
  statusWin <- newWindow 1 cols (rows-1) 0
  statusPan <- newPanel statusWin

  wallColorID    <- newColorID ColorYellow ColorWhite 1
  freeColorID    <- newColorID ColorYellow ColorWhite 2
  scoutColorID   <- newColorID ColorRed ColorWhite 3
  bansheeColorID <- newColorID ColorCyan ColorWhite 4
  tvColorID      <- newColorID ColorGreen ColorWhite 4

  timeRef <- liftIO $ newIORef 0
  let pathLen = length locs
      (tvx,tvy) = castleTV castle
      ((1,1),(width,height)) = bounds $ castleFields castle
      forward s  = liftIO $ modifyIORef timeRef (min (pathLen-1) . (+s))
      backward s = liftIO $ modifyIORef timeRef (max 0 . (+(-s)))
      period = length slices

      loop = do
        redraw
        Just ev <- getEvent topWin Nothing
        case ev of
          EventCharacter 'q' -> return ()
          EventSpecialKey KeyRightArrow -> forward 1 >> loop
          EventSpecialKey KeyLeftArrow -> backward 1 >> loop
          _ -> loop

      redraw = do
        t <- liftIO $ readIORef timeRef
        let Slice slice = slices !! (t `mod` period)
            (px,py) = locs !! t

        updateWindow mapWin $ do
          forM [1..width] $ \x -> do
            forM [1..height] $ \y -> do
              moveCursor (fromIntegral y) (fromIntegral x)
              let (char,colorID) = case slice ! (x,y) of
                    FreeSF -> ('.',freeColorID)
                    WallSF -> ('X',wallColorID)
                    ScoutSF _ -> ('@',scoutColorID)
              setColor colorID
              drawString [char]

          moveCursor (fromIntegral tvy) (fromIntegral tvx)
          setColor tvColorID
          drawString "#"

          moveCursor (fromIntegral py) (fromIntegral px)
          setColor bansheeColorID
          drawString "&"

        updateWindow statusWin $ do
          moveCursor 0 0
          drawString . concat $ ["Step ",show t,"/",show (pathLen-1)]

        refreshPanels
        render

  loop
\end{code}
