module Main (main) where

import Lib

main :: IO ()
main = do
    pGame <- playerGame
    displayGame $ pGame
    currGame <- game
    displayGame $ currGame
    --let count = countSquares (placeRandomList game (mines) mine) mine
    --putStrLn (show count)

