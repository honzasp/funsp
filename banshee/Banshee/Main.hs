module Banshee.Main where
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

data Flag
  = HelpFlag
  | NotThroughFlag
  | QuietFlag
  deriving (Eq,Show)

options =
  [ Option ['h','?'] ["help","use"] (NoArg HelpFlag) 
    "show the help"
  , Option ['n'] ["not-through"] (NoArg NotThroughFlag)
    "disable going through the walls"
  , Option ['q'] ["quiet"] (NoArg QuietFlag)
    "show just the length of the found path"
  ]

header progname = "Usage: " ++ progname ++ " [-hniq] castle-file"

main = do
  progname <- getProgName
  let usage = usageInfo (header progname) options
  (flags,files,errs) <- getOpt Permute options <$> getArgs

  if not (null errs) then do
      hPutStrLn stderr usage
      hPutStrLn stderr $ concat errs
      exitFailure
    else return ()

  let thruWalls = NotThroughFlag `notElem` flags
      quiet = QuietFlag `elem` flags
      help = HelpFlag `elem` flags

  if help then do
      putStrLn usage
      exitSuccess
    else return ()

  if length files /= 1 then do
      hPutStrLn stderr usage
      hPutStrLn stderr "Expected one input file with castle"
      exitFailure
    else return ()

  txt <- readFile (head files)
  castle <- case parseCastle (head files) txt of
      Right c -> return c
      Left err -> do
        hPutStrLn stderr (show err)
        exitFailure

  let slices = sliceCastle castle
      mpath = navigate castle slices thruWalls

  case mpath of
    Just locs
      | quiet       -> showQuiet castle locs
      | otherwise   -> showPath castle locs
    Nothing ->
        putStrLn $ "No path found"
        
  return ()

showQuiet :: Castle -> [Loc] -> IO ()
showQuiet castle locs = 
  putStrLn . concat $ ["Found a path with ",show steps," steps",
    " (",show thruWalls," through walls)"]
  where
  steps = length locs
  fields = castleFields castle
  thruWalls = length $ filter ((==Wall) . (fields !)) locs

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

