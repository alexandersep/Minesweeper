module Main (main) where

import Lib

main :: IO ()
main = do
    pGame <- playerGame
    currGame <- game
    displayGame $ currGame
    gameLoop pGame currGame
