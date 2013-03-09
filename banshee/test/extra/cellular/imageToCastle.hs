import Graphics.GD
import System.Environment
import System.IO
import System.FilePath
import Control.Monad

main = do
  [imageName] <- getArgs
  let castleName = replaceExtension imageName ".in"
  img <- loadPngFile imageName
  cs <- openFile castleName WriteMode
  
  (width,height) <- imageSize img
  hPutStrLn cs . concat $ [show width," ",show height]
  forM_ [0..height-1] $ \y -> do
    forM_ [0..width-1] $ \x -> do
      pix <- getPixel (x,y) img
      hPutStr cs $ if pix > 0x888888 then "." else "X"
    hPutStrLn cs ""

  hClose cs
