module Krunimir.MainPlus (main) where
import System.Environment (getArgs,getProgName)
import System.IO (stderr,hPutStrLn)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)
import Data.Time.Clock (getCurrentTime,diffUTCTime)

import Krunimir.Parser(parse)
import Krunimir.Evaluator(eval)
import Krunimir.Renderer(render)
import Krunimir.SVGRenderer(renderSVG)
import Krunimir.Trace(prune)

renderPNG = render

main :: IO ()
main = do
  startTime <- getCurrentTime
  args <- getArgs
  (inputFile,steps) <- case args of
    [file] -> return (file,Nothing)
    [file,steps] -> return (file,Just $ read steps)
    _ -> do
      progname <- getProgName
      hPutStrLn stderr $ "Use: " ++ progname ++ " input-file [steps]"
      exitFailure

  txt <- readFile inputFile
  ast <- case parse inputFile txt of
    Right ast -> return ast
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure

  let fullTrace = eval ast
      prunedTrace = case steps of
        Nothing -> fullTrace 
        Just count -> prune count fullTrace
      outputFileSvg = replaceExtension inputFile ".test.svg"
      outputFilePng = replaceExtension inputFile ".test.png"

  renderSVG prunedTrace outputFileSvg
  renderPNG prunedTrace outputFilePng

  endTime <- getCurrentTime
  putStrLn $ show (diffUTCTime endTime startTime) ++ " : " ++ inputFile
