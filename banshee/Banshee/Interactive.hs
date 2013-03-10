{-# LANGUAGE CPP #-}
module Banshee.Interactive (showInteractive) where

#ifndef ENABLE_INTERACTIVE
import System.IO
import Banshee.Castle

showInteractive :: Castle -> [Slice] -> Maybe [Loc] -> IO ()
showInteractive _ _ _ = 
  hPutStrLn stderr "banshee was build with disabled support for interactive mode"
#else

import Control.Monad
import Control.Monad.IO.Class
import Data.Array
import Data.IORef
import UI.NCurses

import Banshee.Castle

showInteractive :: Castle -> [Slice] -> Maybe [Loc] -> IO ()
showInteractive _ _ Nothing = 
  putStrLn "No path found"
showInteractive castle slices (Just locs) = runCurses $ do
  win <- defaultWindow
  _ <- setCursorMode CursorInvisible
  setEcho False

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
                forM_ [0..mcols-1] $ \col ->
                  forM_ [0..mrows-1] $ \row -> do
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
              drawString . take (fromInteger cols-1) . concat $
                ["Step ",show t,"/",show (pathLen-1)," (Press ? for help)"]

        updateWindow win updateMap
        updateWindow win updateStatus
        render

  let helpLines = 
        [ ""
        , "    (Press ESC to close)"
        , ""
        , "  q      - quit the program"
        , "  left   - one step backward"
        , "  right  - one step forward"
        , "  page down - five steps forward"
        , "  page up   - five steps backward"
        , "  space  - start autoplay mode (then any key to leave)"
        ]


  let loop :: Curses ()
      loop = do
        redraw
        Just ev <- getEvent win Nothing
        case ev of
          EventCharacter 'q' -> return ()
          EventCharacter '?' -> showHelp >> loop
          EventCharacter ' ' -> playLoop >> loop
          EventSpecialKey KeyRightArrow   -> forward 1  >> loop
          EventSpecialKey KeyLeftArrow    -> backward 1 >> loop
          EventSpecialKey KeyNextPage     -> forward 5  >> loop
          EventSpecialKey KeyPreviousPage -> backward 5 >> loop
          EventResized -> loop
          _ -> loop

        where

        forward s  = liftIO $ modifyIORef timeRef (min (pathLen-1) . (+s))
        backward s = liftIO $ modifyIORef timeRef (max 0 . (+(-s)))

        showHelp = do
          (rows,cols) <- screenSize
          let hrows = rows-4
              hcols = cols-8

          helpWin <- newWindow hrows hcols 2 4
          updateWindow helpWin $ do
            setColor normalColorID

            forM_ [0..hrows-1] $ \row -> do
              moveCursor row 0
              drawString $ replicate (fromIntegral hcols-1) ' '

            drawBox Nothing Nothing
            forM_ (zip [0..] $ take (fromInteger hrows) helpLines) $ \(row,helpLine) -> do
              moveCursor row 1
              drawString $ take (fromInteger hcols-2) helpLine

          render
          closeWindow helpWin
          helpLoop

        helpLoop :: Curses ()
        helpLoop = do
          Just ev <- getEvent win Nothing
          case ev of
            EventResized -> redraw >> showHelp
            EventCharacter '\ESC' -> return ()
            EventCharacter 'q' -> return ()
            _ -> helpLoop

        playLoop :: Curses ()
        playLoop = do
          t <- liftIO $ readIORef timeRef
          if t < pathLen-1 then do
              mev <- getEvent win $ Just 100
              case mev of
                Nothing -> forward 1 >> redraw >> playLoop
                Just (EventCharacter _) -> return ()
                Just (EventSpecialKey _) -> return ()
                Just EventResized -> redraw >> playLoop
                _ -> playLoop
            else return ()

  loop

#endif
