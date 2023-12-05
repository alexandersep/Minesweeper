module Lib
    ( setup, displayGame, 
      place, 
      empty, mine, hidden, number,
      gameState, playerGameState, replaceSquare, replaceRow, 
      generateMines, randomLocations,
      currTime, placeRandomList, countSquares,
      playerGame, game, 
      revealSquare, gameLoop,
      incrementSurroundings, incrementSquare, placeNumbers,
      revealNeighbours, revealSquares
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
empty, mine, hidden :: Square
empty = Empty
mine = Mine
hidden = Hidden
number :: Int -> Square
number x = Number x

-- Minesweeper functionality
setup :: Width -> Height -> Square -> Game
setup w h sq = ([[sq | _ <- [0..w-1]] | _ <- [0..h-1]], (w,h))

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

placeLocations :: Game -> [Location] -> Game
placeLocations game [] = game
placeLocations game (l:ls) = placeLocations rv ls 
    where rv = case revealSquare game l of
                    Just x  -> place game l x
                    Nothing -> game

revealSquare :: Game -> Location -> Maybe Square
revealSquare (b, (w,h)) (x,y)
 | x <= (w-1) && x >= 0 && y <= (h-1) && y >= 0 = Just $ b !! x !! y
 | otherwise = Nothing

revealSquares :: Game -> [Location] -> [Square]
revealSquares _ [] = []
revealSquares game ((x,y):ls) = case revealSquare game (y,x) of
                                    Just x -> x : revealSquares game ls
                                    Nothing -> revealSquares game ls

-- Game -> Empty Location -> Game
revealNeighbours :: Game -> Location -> [Location]
revealNeighbours game l 
 | not (isMinelessSquare game l) = [l] -- It's a MINE!
 | otherwise = DL.nub (l : revealNeighbours' game [l] [])

revealNeighbours' :: Game -> [Location] -> [Location] -> [Location] 
revealNeighbours' _ [] _ = [] 
revealNeighbours' game@(b,(w,h)) ((x,y):ls) visited
 | x <= (w-1) && x >= 0 && y <= (h-1) && y >= 0 && minelessNeighbours game (x,y) && not (elem (x,y) visited) = 
    (x,y) : revealNeighbours' game ((x-1,y-1):(x,y-1):(x+1,y-1):(x-1,y):(x+1,y):(x-1,y+1):(x,y+1):(x+1,y+1):ls) ((x,y):visited)
 | isMinelessSquare game (x,y) && b && not (elem (x,y) visited) = (x,y) : revealNeighbours' game ls ((x,y):visited)
 | otherwise = revealNeighbours' game ls ((x,y):visited)
    where rv = revealSquare game (x,y)
          b  = case rv of
                Just x  -> True 
                Nothing -> False
 
isMinelessSquare :: Game -> Location -> Bool
isMinelessSquare game (x,y) = case revealSquare game (x,y) of
                                Just j -> case j of
                                    Empty    -> True
                                    Hidden   -> False -- Unknown
                                    Mine     -> False
                                    Number n -> True
                                Nothing -> True -- Not a valid coordinate

minelessNeighbours :: Game -> Location -> Bool
minelessNeighbours game (x,y) = g && g1 && g2 && g3 && g4 && g5 && g6 && g7
    where g  = isMinelessSquare game (x-1,y-1) -- up Left
          g1 = isMinelessSquare game (x,y-1)   -- up middle
          g2 = isMinelessSquare game (x+1,y-1) -- up right
          g3 = isMinelessSquare game (x-1,y)   -- middle left 
          g4 = isMinelessSquare game (x+1,y)   -- middle right
          g5 = isMinelessSquare game (x-1,y+1) -- down left
          g6 = isMinelessSquare game (x,y+1)   -- down middle
          g7 = isMinelessSquare game (x+1,y+1) -- down right

placeRandomList :: Game -> [Location] -> Square -> Game
placeRandomList (b, l) [] _ = (b,l)
placeRandomList (b, l) (x:xs) sq = placeRandomList (place (b, l) x sq) xs sq

countSquares :: Game -> Square -> Int
countSquares (b, _) sq = sum $ map (length . filter (==sq)) b

incrementSquare :: Game -> Location -> Game
incrementSquare game (x,y) = do
    let rsq = revealSquare game (x,y)
    case rsq of
        Just j -> do
            case j of
               Empty    -> place game (x,y) (Number 1) -- Empty means Number 0 technically
               Mine     -> game
               Hidden   -> game
               Number n -> place game (x,y) (Number (n+1))
        Nothing -> game

incrementSurroundings :: Game -> Location -> Game
incrementSurroundings game (x,y) = g7
    where g  = incrementSquare game (x-1,y-1) -- up Left
          g1 = incrementSquare g (x,y-1)      -- up middle
          g2 = incrementSquare g1 (x+1,y-1)   -- up right
          g3 = incrementSquare g2 (x-1,y)     -- middle left 
          g4 = incrementSquare g3 (x+1,y)     -- middle right
          g5 = incrementSquare g4 (x-1,y+1)   -- down left
          g6 = incrementSquare g5 (x,y+1)     -- down middle
          g7 = incrementSquare g6 (x+1,y+1)   -- down right

-- Game -> Mine Locations -> Game
placeNumbers :: Game -> [Location] -> Game
placeNumbers game [] = game
placeNumbers game ((x,y):ms) = placeNumbers (incrementSurroundings game (x,y)) ms

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

gameState :: Game
gameState = setup 3 3 Empty

playerGameState:: Game
playerGameState = setup 3 3 Hidden

playerGame :: IO Game
playerGame = do
    putStrLn "Player Game State"
    return $ playerGameState

game :: IO Game
game = do
    putStrLn "Game State"
    let (_, (x, y)) = gameState
    mines <- generateMines (x,y) 1
    let mineGame   = placeRandomList gameState (mines) Mine
    let numberGame = placeNumbers mineGame mines
    return $ numberGame

gameLoop :: Game -> Game -> IO ()
gameLoop pG lG = do
    displayGame pG
    putStr "Give x coordinate "
    y <- getLine -- I know this is weird, by coordinatates are backwards
    putStr "Give y coordinate "
    x <- getLine
    let xInt = read x :: Int
    let yInt = read y :: Int
    let sq = revealSquare lG (xInt, yInt)
    case sq of
        Just a  -> do
            putStrLn ("The Square is " ++ (show a))
            case a of
                Mine -> do
                    let updatedGame = (place pG (xInt,yInt) a)
                    displayGame updatedGame
                    putStrLn "Game Over"
                _    -> do
                    let updatedGame = (place pG (xInt,yInt) a)
                    let rN = revealNeighbours updatedGame (xInt, yInt)
                    putStrLn (show rN)
                    let newestGame = placeLocations updatedGame rN
                    gameLoop newestGame lG
        Nothing -> do
            putStrLn "(x,y) Given was invalid, try again"
            gameLoop pG lG
