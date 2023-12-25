{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main (main) where

import Lib -- import our minesweeper library

-- Threepenny related imports
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Widgets
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Canvas

-- Needed for state control (controlling my variables this way) (liftIO, writeIORef, readIORef)
import Control.Monad
import Data.IORef

-- These modes decide what I want to do when I click on one of the squares (tiles)
data Modes = SelectIt
           | MarkIt
           deriving (Show, Eq)

-- Main function, calls uisetup and gets the threepenny programme started
main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "static"
        } $ uisetup

-- uisetup, this is where the threepenny magic happens
uisetup :: Window -> UI ()
uisetup window = do
    return window # set title "Minesweeper in Haskell"

    -- Include external CSS file
    addStyleSheet window "style.css"

    -- Set buttons for different levels the player can choose
    let options = ["beginner", "intermediate", "expert"]
    select <- UI.select #+ Prelude.map (\op -> UI.option # set UI.value op #+ [string op]) options
    begin  <- UI.button #+ [string "begin"]
    reset <- UI.div #+ [string "reset"]

    -- Arrange buttons in layout
    --getBody window #+ [grid [[element select, element begin]]]
    getBody window #+ [element select, element begin]

    -- Define behavior for buttons begin. Which is where most of the logic is found
    on UI.click begin $ \_ -> do
        mVal <- get value select
        --liftIO $ putStrLn $ "Selected level: " ++ mVal
        let (w,h,m) = chooseBoard mVal
    
        -- Referenced: https://stackoverflow.com/questions/52323232/drawing-an-image-onto-a-threepenny-gui-canvas
        canvas <- UI.canvas
            # set UI.height (w * 25)
            # set UI.width (h * 25)
            # set style [("position", "absolute"), ("left", "0px"), ("top", "0px"), ("border", "solid black 1px"), ("background", "#c0c0c0")]


        lG <- liftIO $ game (h,w) m
        pG <- liftIO $ playerGame (h,w)

        -- Draw the initial grid with all hidden elements (draws the pG player game board which for the player should all be hidden items)
        webDrawGame pG canvas

        -- create references so I can refer to them and modify them like variables
        lGRef <- liftIO $ newIORef $ lG 
        pGRef <- liftIO $ newIORef $ pG 
        playingRef <- liftIO $ newIORef True

        xPosRef <- liftIO $ newIORef 0
        yPosRef <- liftIO $ newIORef 0

        mark <- UI.button #+ [string "mark"]
        sel <- UI.button #+ [string "select"]
        solve <- UI.button #+ [string "solve"]

        getBody window 
            # set children [canvas, mark, sel, solve]
        element mark   # set style [("position", "absolute"), ("left", "0px"), ("top", (show (w * 26)) ++ "px")]
        element sel    # set style [("position", "absolute"), ("left", (show (1.65 * 26)) ++ "px"), ("top", (show (w * 26)) ++ "px")]
        element solve  # set style [("position", "absolute"), ("left", (show (3.45 * 26)) ++ "px"), ("top", (show (w * 26)) ++ "px")]

        let gameSetup = setup w h Lib.Empty
        --liftIO $ displayGame $ lG

        mode <- liftIO $ newIORef SelectIt
        
        on UI.click solve $ \_ -> do -- solve
            pG <- liftIO $ readIORef pGRef
            lG <- liftIO $ readIORef lGRef
            let solving = solver pG lG
            if anySquare solving Mine
                then gameOver window lG playingRef canvas
            else if match ((locateSquares pG Lib.Mark) ++ (locateSquares pG Lib.Hidden)) (locateSquares lG Lib.Mine) -- check to see if win condition is right
                then do
                    youWin window lG playingRef canvas
            else do
                UI.clearCanvas canvas
                webDrawGame solving canvas
            liftIO $ writeIORef pGRef (solving)

        on UI.mousedown canvas $ \(x,y) -> do
            playing <- liftIO $ readIORef playingRef
            if not playing
                then return () 
                else do
                    let posX = min (floor (x / 25)) h -- min because the canvas can technically have out of index in the outer edges 
                    let posY = min (floor (y / 25)) w

                    pG <- liftIO $ readIORef pGRef
                    lG <- liftIO $ readIORef lGRef

                    --liftIO $ displayGame $ solver pG lG
                    --liftIO $ putStrLn $ show $ giveLocationsExceptMark pG (posX, posY)
                    --liftIO $ displayGame $ pG
                    let (tempB,(w,h)) = lG
                    let sq = revealSquare lG (posX,posY) -- reveal square of the game the player hasn't seen
                    case sq of
                        Nothing -> do -- If the square isn't a valid coordinate (this should never execute)
                            --liftIO $ putStrLn "It's an invalid coordinate"
                            invalid <- UI.div #+ [string "Invalid coordinate selected"]
                            getBody window # set children [invalid]
                            return ()
                        Just a -> do -- If valid coordinate
                            myMode <- liftIO $ readIORef mode
                            --liftIO $ putStrLn $ (show a)
                            if myMode == MarkIt -- If player selected button to mark it
                                then do
                                    --liftIO $ putStrLn "It's MarkIt"
                                    let psq = revealSquare pG (posX, posY)
                                    if psq == Just Lib.Mark
                                        then do
                                            --liftIO $ putStrLn "psq is Just Mark"
                                            let newpG = (place pG (posX, posY) Hidden)
                                            liftIO $ writeIORef pGRef (newpG)
                                            UI.clearCanvas canvas
                                            webDrawGame newpG canvas
                                        else if psq == Just Lib.Hidden then do 
                                            --liftIO $ putStrLn "Just not Mark"
                                            let newpG = (place pG (posX,posY) Mark)
                                            liftIO $ writeIORef pGRef (newpG)
                                            UI.clearCanvas canvas
                                            webDrawGame newpG canvas
                                        else do
                                            --liftIO $ putStrLn "Not hidden in markit"
                                            return ()
                                else do -- player selected Select 
                                    case a of
                                        Lib.Mine -> do -- If square is a mine
                                            gameOver window lG playingRef canvas
                                        _    -> do -- if square is not a mine
                                            --liftIO $ putStrLn "Not a mine"
                                            updatedPG <- liftIO $ readIORef pGRef
                                            let updatedGame = (place updatedPG (posX,posY) a)
                                            let rN = revealNeighbours lG (posX,posY)
                                            let newestGame = placeLocations updatedGame lG rN
                                            liftIO $ writeIORef pGRef (newestGame)
                                            pG <- liftIO $ readIORef pGRef
                                            --liftIO $ displayGame lG -- Debugging
                                            UI.clearCanvas canvas
                                            webDrawGame pG canvas
                                            if match ((locateSquares pG Lib.Mark) ++ (locateSquares pG Lib.Hidden)) (locateSquares lG Lib.Mine) -- check to see if win condition is right
                                                then do 
                                                    youWin window lG playingRef canvas
                                                else return ()

        on UI.click mark $ \_ -> do -- mark button mode selected
            --liftIO $ putStrLn "MarkedIt"
            liftIO $ writeIORef mode MarkIt
        
        on UI.click sel $ \_ -> do -- select button mode selected
            --liftIO $ putStrLn "SelectIt"
            liftIO $ writeIORef mode SelectIt

-- Game over screen
gameOver :: Window -> Game -> (IORef Bool) -> Canvas -> UI ()
gameOver window lG@(b,(w,h)) playingRef canvas = do
    yl <- UI.div #+ [string "You lose"]
    element yl # set style [("position", "absolute"), ("left", "0px"), ("top", (show (h * 26)) ++ "px")]
    UI.clearCanvas canvas
    webDrawGame lG canvas
    getBody window # set children [canvas, yl]
    liftIO $ writeIORef playingRef False
    return ()

-- You win screen
youWin :: Window -> Game -> (IORef Bool) -> Canvas -> UI ()
youWin window lG@(b,(w,h)) playingRef canvas = do
    yw <- UI.div #+ [string "You win"]
    element yw # set style [("position", "absolute"), ("left", "0px"), ("top", (show (h * 26)) ++ "px")]
    UI.clearCanvas canvas
    webDrawGame lG canvas
    getBody window # set children [canvas, yw]
    liftIO $ writeIORef playingRef False
    return ()

-- (width,height,numberOfMines)
-- Difficulty level
chooseBoard :: String -> (Int,Int,Int)
chooseBoard mVal
 | mVal == "expert"       = (16,30,99)
 | mVal == "intermediate" = (16,16,40)
 | otherwise              = (9,9,10)

-- Draw a row on a canvas
-- Helper function for webDrawGame' which is another helper function
webDrawRow :: (Int, Int) -> Game -> Canvas -> UI ()
webDrawRow (_,(-1)) _ _ = return () 
webDrawRow (x,y) game@(b, (w,h)) canvas = do
    case b !! y !! x of
        Lib.Empty    -> UI.fillText " " ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas
        Lib.Mine     -> UI.fillText "💣" ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas 
        Lib.Hidden   -> UI.fillText "⚪" ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas
        Lib.Number n -> UI.fillText (show n) ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas
        Lib.Mark     -> UI.fillText "⛳" ((fromIntegral x+0.33) * 25, (fromIntegral y+0.66) * 25) canvas
    webDrawRow (x, y-1) game canvas

-- Draw the whole game on the canvas (not the grid) but the items themselves.
-- Notice how this function calls webDrawRow it's helper function for drawing rows
-- Helper function to webDrawGame
webDrawGame' :: Game -> Canvas -> UI ()
webDrawGame' (_, ((-1),_)) _ = return ()
webDrawGame' game@(b, (x,y)) canvas = do 
    webDrawRow (x,y) game canvas
    webDrawGame' (b, (x-1,y)) canvas 

-- Draw the board with the elements drawn in given a Game (board and width and height)
webDrawGame :: Game -> Canvas -> UI ()
webDrawGame (b, (x,y)) canvas = webDrawGrid canvas (x,y) >> webDrawGame' (b, (x-1,y-1)) canvas

-- Draws horizontal lines on a canvas (the horizontal lines of a grid)
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

-- Draws vertical lines on a canvas (the vertical lines of a grid)
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

-- Draw the grid lines, vertical and horizontal
webDrawGrid :: Canvas -> (Int, Int) -> UI ()
webDrawGrid canvas (x,y) = webDrawHorz canvas (x,y) >> webDrawVert canvas (x,y)
