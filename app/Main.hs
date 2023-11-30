module Main (main) where

import Lib

main :: IO ()
main = do
    let (_,(x,y)) = game    
    mines <- generateMines (x,y) 8
    displayGame $ placeRandomList game (mines) mine
    --let count = countSquares (placeRandomList game (mines) mine) mine
    --putStrLn (show count)
