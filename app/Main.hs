{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main (main) where

import Lib

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Widgets
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Canvas

import Control.Monad
import Data.IORef

data Modes = SelectIt
           | MarkIt
           deriving (Show, Eq)

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "static"
        } $ uisetup

uisetup :: Window -> UI ()
uisetup window = do
    return window # set title "Minesweeper in Haskell"

    -- Include external CSS file
    addStyleSheet window "style.css"

    let options = ["beginner", "intermediate", "expert"]
    select <- UI.select #+ Prelude.map (\op -> UI.option # set UI.value op #+ [string op]) options
    begin  <- UI.button #+ [string "begin"]
    reset <- UI.div #+ [string "reset"]

    -- Arrange buttons in layout
    getBody window #+ [grid [[element select, element begin]]]

    -- Define behavior for buttons
    on UI.click begin $ \_ -> do
        mVal <- get value select
        liftIO $ putStrLn $ "Selected level: " ++ mVal
        let (h,w,m) = chooseBoard mVal
    
        -- Referenced: https://stackoverflow.com/questions/52323232/drawing-an-image-onto-a-threepenny-gui-canvas
        canvas <- UI.canvas
            # set UI.height (h * 25)
            # set UI.width (w * 25)
            # set style [("position", "absolute"), ("left", "0px"), ("top", "0px"), ("border", "solid black 1px"), ("background", "#eee")]

        webDrawGrid canvas (w,h)

        --UI.fillText "1" (0.33 * 25, 0.66 * 25) canvas
        --UI.fillText "1" ((fromIntegral w-1+0.33) * 25, (fromIntegral h-1+0.66) * 25) canvas
        --UI.fillText "8" (0 * 19, 3 * 19) canvas
        --UI.fillText "2" (1 * 19, 2 * 19) canvas
        --UI.fillText "3" (2 * 19, 2 * 19) canvas
        --UI.fillText "4" (3 * 19, 3 * 19) canvas
        --UI.fillText "5" (4 * 19, 4 * 19) canvas
        --UI.fillText "5" (8 * 19, 8 * 19) canvas
        --UI.clearCanvas canvas

        lG <- liftIO $ game (w,h)
        pG <- liftIO $ playerGame (w,h)
        wpG <- webplace pG

        webDrawGame lG canvas

        lGRef <- liftIO $ newIORef $ lG 
        pGRef <- liftIO $ newIORef $ pG 
        wpGRef <- liftIO $ newIORef $ wpG 

        mark <- UI.button #+ [string "mark"]
        sel <- UI.button #+ [string "select"]

        startingWPG <- liftIO $ readIORef wpGRef
        let layout = grid $ Prelude.map (Prelude.map element) startingWPG
        lay <- layout
            # set style [("opacity", "0.5")]
        getBody window # set children [canvas, mark, sel]

        let gameSetup = setup w h Lib.empty
        liftIO $ displayGame $ lG

        mode <- liftIO $ newIORef SelectIt

        --Set up click event handlers for each button
        forM_ [0..w-1] $ \x ->
            forM_ [0..h-1] $ \y -> do
                updatedWpG <- liftIO $ readIORef wpGRef
                on UI.click (updatedWpG !! y !! x) $ \_ -> do
                    if match ((locateSquares pG Lib.Mark) ++ (locateSquares pG Lib.Hidden)) (locateSquares lG Lib.Mine) 
                        then do 
                            uw <- UI.div #+ [string "You win"]
                            getBody window # set children [uw]
                            return ()
                        else do
                            let sq = revealSquare lG (x,y)
                            case sq of
                                Nothing -> do
                                    liftIO $ putStrLn "It's an invalid coordinate"
                                    invalid <- UI.div #+ [string "Invalid coordinate selected"]
                                    getBody window # set children [invalid]
                                    return ()
                                Just a -> do
                                    myMode <- liftIO $ readIORef mode
                                    if myMode == MarkIt
                                        then do
                                            liftIO $ putStrLn "It's MarkIt"
                                            let psq = revealSquare pG (x, y)
                                            if psq == Just Lib.Mark
                                                then do
                                                    liftIO $ putStrLn "psq is Just Mark"
                                                    --wpG <- webplace (place pG (x,y) Hidden)
                                                    --liftIO $ writeIORef wpG (webplace (place pG (x,y) Hidden))
                                                    --let newpG = (place pG (x,y) Hidden)
                                                    --liftIO $ displayGame newpG
                                                    --newwpG <- webplace pG
                                                    --let layout = grid $ Prelude.map (Prelude.map element) newwpG
                                                    --lay <- layout
                                                    --getBody window # set children [lay, mark, sel]
                                                    --getBody window #+ [element lay]
                                                    return ()
                                                else do
                                                    liftIO $ putStrLn "Just not Mark"
                                                    let newpG = (place pG (x,y) Mark)
                                                    --liftIO $ displayGame newpG
                                                    --liftIO $ putStrLn "I should've marked something"
                                                    --wpG <- webplace newpG
                                                    --let layout = grid $ Prelude.map (Prelude.map element) wpG
                                                    --lay <- layout
                                                    --getBody window # set children [lay, mark, sel]
                                                    return ()
                                        else do -- mode is select
                                            case a of
                                                Lib.Mine -> do
                                                    liftIO $ putStrLn "It's a mine!"
                                                    go <- UI.div #+ [string "Game Over"]
                                                    getBody window # set children [go]
                                                    return ()
                                                _    -> do
                                                    liftIO $ putStrLn "Not a mine"
                                                    updatedPG <- liftIO $ readIORef pGRef
                                                    let updatedGame = (place updatedPG (x,y) a)
                                                    let rN = revealNeighbours lG (x,y)
                                                    let newestGame = placeLocations updatedGame lG rN
                                                    liftIO $ writeIORef wpGRef =<< runUI window (webplace newestGame)

                                                    mywpG <- liftIO $ readIORef wpGRef 
                                                    let layout = grid $ Prelude.map (Prelude.map element) mywpG
                                                    lay <- layout
                                                    liftIO $ putStrLn "I Got here"
                                                    getBody window #+ [element lay, element mark, element sel]
                                                    --getBody window # set children [lay, mark, sel]
                                                    --getBody window #+ [element lay]
                                                    return ()

        on UI.click mark $ \_ -> do
            liftIO $ putStrLn "MarkedIt"
            liftIO $ writeIORef mode MarkIt
        
        on UI.click sel $ \_ -> do
            liftIO $ putStrLn "SelectIt"
            liftIO $ writeIORef mode SelectIt

-- (width,height,numberOfMines)
--chooseBoard :: MonadIO m => String -> (Int,Int,Int) 
chooseBoard :: String -> (Int,Int,Int)
chooseBoard mVal
 | mVal == "expert"       = (16,30,99)
 | mVal == "intermediate" = (16,16,40)
 | otherwise              = (9,9,10)


webDrawRow :: (Int, Int) -> Game -> Canvas -> UI ()
webDrawRow ((-1),_) _ _ = return () 
webDrawRow (x,y) game@(b, (w,h)) canvas = do
    case b !! y !! x of
        Lib.Empty    -> UI.fillText "e" ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas
        Lib.Mine     -> UI.fillText "m" ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas
        Lib.Hidden   -> UI.fillText "h" ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas
        Lib.Number n -> UI.fillText (show n) ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas
        Lib.Mark     -> UI.fillText "x" ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas
    webDrawRow (x-1, y) game canvas

webDrawGame' :: Game -> Canvas -> UI ()
webDrawGame' (_, (_,(-1))) _ = return ()
webDrawGame' game@(b, (x,y)) canvas = do 
    webDrawRow (x,y) game canvas
    webDrawGame' (b, (x,y-1)) canvas 

webDrawGame :: Game -> Canvas -> UI ()
webDrawGame (b, (x,y)) canvas = webDrawGame' (b, (x-1,y-1)) canvas

webSetRow' :: Row -> [UI Element]
webSetRow' [x] = return $ case x of
    Lib.Empty    -> UI.button # set style [("color", "blue")]   #+ [string "e"] 
    Lib.Mine     -> UI.button # set style [("color", "red")]    #+ [string "b"] 
    Lib.Hidden   -> UI.button # set style [("color", "green")]  #+ [string "h"] 
    Lib.Number n -> UI.button # set style [("color", "orange")] #+ [string (show n)] 
    Lib.Mark     -> UI.button # set style [("color", "black")]  #+ [string "x"]
webSetRow' (x:xs) = case x of
    Lib.Empty    -> UI.button # set style [("color", "blue")]   #+ [string "e"] : webSetRow' xs
    Lib.Mine     -> UI.button # set style [("color", "red")]    #+ [string "b"] : webSetRow' xs
    Lib.Hidden   -> UI.button # set style [("color", "green")]  #+ [string "h"] : webSetRow' xs
    Lib.Number n -> UI.button # set style [("color", "orange")] #+ [string (show n)] : webSetRow' xs
    Lib.Mark     -> UI.button # set style [("color", "black")]  #+ [string "x"] : webSetRow' xs

webSetRow :: Row -> UI [Element]
webSetRow xs = sequence $ webSetRow' xs

-- Location will always be within bounds but in case not nothing will occur
webplace :: Game -> UI [[Element]]
webplace (b, (w,h)) = sequence $ Prelude.map (webSetRow) b

webDrawHorz :: Canvas -> (Int, Int) -> UI ()
webDrawHorz _ (_,0) = return () 
webDrawHorz canvas (x,y) = do
        let bx = (fromIntegral x) -- betweenX
        let by = (fromIntegral y) -- betweenY
        UI.beginPath canvas
        UI.moveTo (0.0*25.0,by*25.0) canvas
        UI.lineTo (bx*25.0,by*25.0) canvas
        UI.closePath canvas
        UI.stroke canvas
        webDrawHorz canvas (x,y-1)

webDrawVert :: Canvas -> (Int, Int) -> UI ()
webDrawVert _ (0,_) = return () 
webDrawVert canvas (x,y) = do
        let bx = (fromIntegral x) -- betweenX
        let by = (fromIntegral y) -- betweenY
        UI.beginPath canvas
        UI.moveTo (bx*25.0,0.0*25.0) canvas
        UI.lineTo (bx*25.0,by*25.0) canvas
        UI.closePath canvas
        UI.stroke canvas
        webDrawVert canvas (x-1,y)

webDrawGrid :: Canvas -> (Int, Int) -> UI ()
webDrawGrid canvas (x,y) = webDrawHorz canvas (x,y) >> webDrawVert canvas (x,y)

--replaceRow :: Location -> Board -> UI [[Element]] 
--replaceRow _ [] = []
--replaceRow (x,0) (z:zs) = replaceSquare x z : zs
--replaceRow (x,y) (z:zs) = z:replaceRow (x,y-1) zs
--
--place :: Game -> Location -> Game
--place (b, l) (x,y) = (replaceRow (x,y) b, l) 

{-
main :: IO ()
main = do
    pGame <- playerGame
    currGame <- game
    gameLoop pGame currGame
-}
