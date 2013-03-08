module Banshee.Interactive (showInteractive) where
import Control.Monad
import Control.Monad.IO.Class
import Data.Array
import Data.IORef
import UI.NCurses

import Banshee.Castle

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

  let redraw :: Curses ()
      redraw = do
        (rows,cols) <- screenSize
        t <- liftIO $ readIORef timeRef

        let Slice slice = slices !! (t `mod` period)
            (bansheeX',bansheeY') = locs !! t
            bansheeX = fromIntegral bansheeX' :: Integer
            bansheeY = fromIntegral bansheeY'

        let updateMap :: Update ()
            updateMap = do
              clear
              forM_ [startX..endX] $ \x ->
                forM_ [startY..endY] $ \y -> 
                  updateField x y
              where

              mrows = rows-1
              mcols = cols

              startX = max 1 $ min (fromIntegral width-mcols+1) (bansheeX-mcols`div`2)
              startY = max 1 $ min (fromIntegral height-mrows+1) (bansheeY-mrows`div`2)
              endX = min (startX+mcols-1) (fromIntegral width)
              endY = min (startY+mrows-1) (fromIntegral height)

              clear = do
                setColor normalColorID
                forM [0..mcols-1] $ \col ->
                  forM [0..mrows-1] $ \row -> do
                    moveCursor row col
                    drawString " "

              updateField x y = do
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


        let updateStatus :: Update ()
            updateStatus = do
              setColor normalColorID
              moveCursor (rows-1) 0
              drawString $ replicate (fromInteger cols-1) ' '
              moveCursor (rows-1) 0
              drawString . concat $ ["Step ",show t,"/",show (pathLen-1)]

        updateWindow win updateMap
        updateWindow win updateStatus
        render

  let loop :: Curses ()
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

  loop
