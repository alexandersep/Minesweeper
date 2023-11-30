module Lib
    ( someFunc, setup, displayGame, 
      place, 
      empty, mine, number,
      game, replaceSquare, replaceRow 
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- type and data definitions
type Width = Int
type Height = Int

data Square = Empty
            | Mine 
            | Number Int
            deriving (Show, Eq)

-- smart functions 
empty, mine :: Square
empty = Empty
mine = Mine
number :: Int -> Square
number x = Number x

type Row = [Square]
type Board = [[Square]]
type Location = (Int, Int)
type Game = (Board, Location)

-- Minesweeper functionality
setup :: Width -> Height -> Game
setup w h = ([[Empty | _ <- [0..w-1]] | _ <- [0..h-1]], (w,h))

game :: Game
game = setup 9 9

replaceSquare :: Square -> Int -> Row -> Row
replaceSquare _ _ [] = []
replaceSquare sq 0 (_:xs) = sq:xs
replaceSquare sq n (x:xs) = x:replaceSquare sq (n-1) xs

replaceRow :: Square -> Location -> Board -> Board 
replaceRow _ _ [] = []
replaceRow sq (0,y) (z:zs) = replaceSquare sq y z : zs
replaceRow sq (x,y) (z:zs) = z:replaceRow sq (x-1,y) zs

-- Location will always be within bounds but in case not nothing will occur
place :: Game -> Location -> Square -> Game
place (b, l) (x,y) sq = (replaceRow sq (x,y) b, l) 

displayGame :: Game -> IO ()
displayGame ([],(x,y)) = putStrLn (show x ++ "x" ++ show y)
displayGame ((x:xs),l) = putStr "[" >>  displayRow x  >> putStrLn "]" >> displayGame (xs,l)
    
displayRow :: Row -> IO ()
displayRow []  = return ()
displayRow [Empty]    = putStr "E" 
displayRow [Mine]     = putStr "M"
displayRow [Number n] = putStr (show n)
displayRow ((Empty):xs)    = putStr "E, " >> displayRow xs
displayRow ((Mine):xs)     = putStr "M, " >> displayRow xs
displayRow ((Number n):xs) = putStr (show n ++ ", ") >> displayRow xs
