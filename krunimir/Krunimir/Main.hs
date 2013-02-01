module Krunimir.Main
( main
)
where
import System.Environment
import System.IO
import System.Exit
import System.FilePath
import Data.Time.Clock

import Krunimir.Parser(parse)
import Krunimir.Evaluator(eval)
import Krunimir.Painter(paint)
import Krunimir.Image(limit,imageNodes,imageToSegss)

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

  (let fullImage = eval ast
       limitedImage = case steps of
         Nothing -> fullImage
         Just count -> limit count fullImage
       outputFile = replaceExtension inputFile ".test.png"
   in paint (imageToSegss limitedImage) outputFile)
-- in putStrLn $ "Image has " ++ show (imageNodes limitedImage) ++ " nodes"

  endTime <- getCurrentTime
  putStrLn $ show (diffUTCTime endTime startTime) ++ " : " ++ inputFile
