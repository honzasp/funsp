import Text.JSON
import System.FilePath
import System.IO
import System.Exit
import System.Process
import System.Directory
import Control.Applicative
import Control.Monad

data PathResult = NoPath | FoundPath Int Int deriving (Eq,Show)

instance JSON PathResult where
  showJSON = undefined
  readJSON (JSArray []) =
    return NoPath
  readJSON (JSArray [steps]) =
    FoundPath <$> readJSON steps <*> pure 0
  readJSON (JSArray [steps,walls]) =
    FoundPath <$> readJSON steps <*> readJSON walls
  readJSON x@(JSObject jsobj) = do
    let assoc = fromJSObject jsobj
    if null assoc then return NoPath
      else do
        steps <- case lookup "steps" assoc of
          Just s -> return s
          Nothing -> fail $ "Expected key 'steps': " ++ show x
        walls <- case lookup "walls" assoc of
          Just s -> return s
          Nothing -> fail $ "Expected key 'walls': " ++ show x
        FoundPath <$> readJSON steps <*> readJSON walls
  readJSON x = fail $ "Bad result format: " ++ show x

data Test = Test FilePath PathResult deriving Show

instance JSON Test where
  readJSON = undefined
  showJSON = undefined
  readJSONs (JSObject jsobj) = do
    forM (fromJSObject jsobj) $ \(file,res) -> do
      result <- readJSON res
      return $ Test file result

main = do
  let resultsFile = "test/results.json"
  ex <- doesFileExist resultsFile
  if ex then return ()
    else do
      hPutStrLn stderr "Unable to find results.json (try running from banshee/ directory)"
      exitFailure

  let bansheeExec = "dist/build/banshee/banshee"
  ex <- doesFileExist bansheeExec
  if ex then return ()
    else do
      hPutStrLn stderr "Unable to find banshee executable (try running from banshee/ directory)"
      exitFailure

  jsonRes <- decode <$> readFile resultsFile
  tests <- case jsonRes >>= readJSONs of
    Error err -> do
      hPutStrLn stderr $ "JSON parse error: " ++ err
      exitFailure
    Ok x -> return x

  forM_ tests $ \(Test file expected) -> do
    outputRes <- decode <$> readProcess bansheeExec ["--json", file] ""
    got <- case outputRes >>= readJSON of
      Error err -> do
        hPutStrLn stderr $ "JSON parse error: " ++ err
        exitFailure
      Ok x -> return x
    if got == expected
      then putStrLn . concat $ [file," OK"]
      else putStrLn . concat $ [file," !! exp ",show expected," got ",show got]

