module Lib
    ( setup, displayGame, 
      place, 
      empty, mine, number,
      game, replaceSquare, replaceRow, 
      generateMines, randomLocations,
      currTime, placeRandomList, countSquares
    ) where

import System.Random
import qualified Data.List as DL
import Data.Time.Clock.POSIX (getPOSIXTime)

-- type and data definitions
type Width = Int
type Height = Int
type Row = [Square]
type Board = [[Square]]
type Location = (Int, Int)
type Game = (Board, Location)

data Square = Empty
            | Mine 
            | Hidden
            | Number Int
            deriving (Show, Eq)

-- smart functions 
empty, mine :: Square
empty = Empty
mine = Mine
number :: Int -> Square
number x = Number x

-- Minesweeper functionality
setup :: Width -> Height -> Game
setup w h = ([[Empty | _ <- [0..w-1]] | _ <- [0..h-1]], (w,h))

revealSquare :: Square -> Location -> Square
revealSquare (Hidden) (x,y) = 
revealSquare sq _ = sq

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

placeRandomList :: Game -> [Location] -> Square -> Game
placeRandomList (b, l) [] _ = (b,l)
placeRandomList (b, l) (x:xs) sq = placeRandomList (place (b, l) x sq) xs sq

countSquares :: Game -> Square -> Int
countSquares (b, _) sq = sum $ map (length . filter (==sq)) b

generateMines :: Location -> Int -> IO [Location]
generateMines l n = do
    rloc <- randomLocations l
    return $ take n $ DL.nub $ rloc

randomLocations :: Location -> IO [Location]
randomLocations (w,h) = do
    myTime <- currTime
    let gen1 = mkStdGen myTime
    let gen2 = mkStdGen (myTime + 1)
    return $ zip (randomRs (0, w-1) gen1) (randomRs (0, h-1) gen2)

currTime :: IO Int
currTime = do
    time <- getPOSIXTime -- IO POSIXTime
    return (round time)

displayGame :: Game -> IO ()
displayGame ([], (x,y)) = putStrLn (show x ++ "x" ++ show y)
displayGame ((l:ls), (x,y)) = putStr "[" >>  displayRow l  >> putStrLn "]" >> displayGame (ls,(x,y))

displayRow :: Row -> IO ()
displayRow []  = return ()
displayRow [Empty]    = putStr "E" 
displayRow [Mine]     = putStr "M"
displayRow [Hidden]   = putStr "H"
displayRow [Number n] = putStr (show n)
displayRow ((Empty):xs)    = putStr "E, " >> displayRow xs
displayRow ((Mine):xs)     = putStr "M, " >> displayRow xs
displayRow ((Hidden):xs)   = putStr "H, " >> displayRow xs
displayRow ((Number n):xs) = putStr (show n ++ ", ") >> displayRow xs
