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
  win <- defaultWindow

  normalColorID  <- newColorID ColorBlack ColorWhite 1
  scoutColorID   <- newColorID ColorRed ColorWhite 2
  bansheeColorID <- newColorID ColorCyan ColorWhite 3
  tvColorID      <- newColorID ColorGreen ColorWhite 4

  timeRef <- liftIO $ newIORef 0
  let pathLen = length locs
      ((1,1),(width,height)) = bounds $ castleFields castle
      period = length slices

      (tvX',tvY') = castleTV castle
      tvX = fromIntegral tvX' :: Integer
      tvY = fromIntegral tvY'

      loop = do
        redraw
        Just ev <- getEvent win Nothing
        case ev of
          EventCharacter 'q' -> return ()
          EventSpecialKey KeyRightArrow -> forward 1 >> loop
          EventSpecialKey KeyLeftArrow -> backward 1 >> loop
          EventResized -> loop
          _ -> loop

        where
        forward s  = liftIO $ modifyIORef timeRef (min (pathLen-1) . (+s))
        backward s = liftIO $ modifyIORef timeRef (max 0 . (+(-s)))


      redraw = do
        (rows,cols) <- screenSize
        t <- liftIO $ readIORef timeRef

        let Slice slice = slices !! (t `mod` period)
            (bansheeX',bansheeY') = locs !! t
            bansheeX = fromIntegral bansheeX' :: Integer
            bansheeY = fromIntegral bansheeY'

        updateWindow win $ do
          let mrows = rows-1
              mcols = cols

          let startX = max 1 $ min (fromIntegral width-mcols+1) (bansheeX-mcols`div`2) :: Integer
              startY = max 1 $ min (fromIntegral height-mrows+1) (bansheeY-mrows`div`2)
              endX = min (startX+mcols-1) (fromIntegral width)
              endY = min (startY+mrows-1) (fromIntegral height)

          setColor normalColorID
          forM [0..mcols-1] $ \col ->
            forM [0..mrows-1] $ \row -> do
              moveCursor row col
              drawString " "

          forM [startX..endX] $ \x -> do
            forM [startY..endY] $ \y -> do
              moveCursor (y-startY) (x-startX)
              let (char,colorID) = 
                    if (x,y) == (tvX,tvY) then ('#',tvColorID)
                    else if (x,y) == (bansheeX,bansheeY) then ('&',bansheeColorID)
                    else case slice ! (fromInteger x,fromInteger y) of
                        FreeSF -> ('.',normalColorID)
                        WallSF -> ('X',normalColorID)
                        ScoutSF _ -> ('@',scoutColorID)
              setColor colorID
              drawString [char]

        updateWindow win $ do  
          setColor normalColorID
          moveCursor (rows-1) 0
          drawString $ replicate (fromInteger cols-1) ' '
          moveCursor (rows-1) 0
          drawString . concat $ ["Step ",show t,"/",show (pathLen-1)]

        render

  loop
\end{code}
