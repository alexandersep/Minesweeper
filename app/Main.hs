module Main (main) where

import Lib

main :: IO ()
main = do
    pGame <- playerGame
    currGame <- game
    gameLoop pGame currGame
