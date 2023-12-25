-- Many of the function exported aren't required, but I did it for testing using stack ghci
module Lib
    ( setup, displayGame, 
      place, 
      gameState, playerGameState, replaceSquare, replaceRow, 
      generateMines, randomLocations,
      currTime, placeRandomList, countSquares,
      playerGame, game, 
      revealSquare, gameLoop,
      incrementSurroundings, incrementSquare, placeNumbers,
      revealNeighbours, revealSquares, placeLocations,
      repeatIncr, printList, locateSquares, match, 
      Game, Board, Row, Square(..),
      findNextHidden, guess, countMarksAroundRegion, giveLocationsExceptMark, anySquare, solver, solveLogic
    ) where

-- Some imports that were used For randomness it was time and Random
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)
-- List for two functions nub and sort, I could only import those, but I've already finished it so I'll just leave it as is.
import qualified Data.List as DL

-- type and data definitions
type Width = Int
type Height = Int
type Row = [Square]
type Board = [[Square]]
type Location = (Int, Int)
type Game = (Board, Location)

-- Data constructor for what a square is (I guess another word for it was tile, but I didn't think of it at the time)
data Square = Empty
            | Mine 
            | Hidden
            | Number Int
            | Mark
            deriving (Show, Eq)

-- Setup helps set the game which is a  2D list and the w,h of the 2D list
-- We pass in square, later we use it for making a board of Hidden items for the players
-- and we use it with Empty to make the board for the logic player temporarily before we fill it with mines and numbers
setup :: Width -> Height -> Square -> Game
setup w h sq = ([[sq | _ <- [0..w-1]] | _ <- [0..h-1]], (w,h))

-- Replace a single square given the index of the row 
replaceSquare :: Square -> Int -> Row -> Row
replaceSquare _ _ [] = []
replaceSquare sq 0 (_:xs) = sq:xs
replaceSquare sq n (x:xs) = x:replaceSquare sq (n-1) xs

-- Replace a row of a board given a square and location and a board
replaceRow :: Square -> Location -> Board -> Board 
replaceRow _ _ [] = []
replaceRow sq (x,0) (z:zs) = replaceSquare sq x z : zs
replaceRow sq (x,y) (z:zs) = z:replaceRow sq (x,y-1) zs

-- Location will always be within bounds but in case not nothing will occur
place :: Game -> Location -> Square -> Game
place (b, l) (x,y) sq = (replaceRow sq (x,y) b, l) 

-- place all the locations given and make a new game, this new game is going to be used as the new playergame (we are using this function as a state for the player)
placeLocations :: Game -> Game -> [Location] -> Game
placeLocations pG _ [] = pG
placeLocations pG lG (l:ls) = case revealSquare lG l of 
                             Just x -> placeLocations (place pG l x) lG ls
                             Nothing -> pG

-- Reveal a single square given a location
revealSquare :: Game -> Location -> Maybe Square
revealSquare (b, (w,h)) (x,y)
 | limit     = Just $ b !! y !! x
 | otherwise = Nothing
    where limit = x <= (w-1) && x >= 0 && y <= (h-1) && y >= 0

-- Reveal the squares given a game and locations. make a list of squares that were revealed
revealSquares :: Game -> [Location] -> [Square]
revealSquares _ [] = []
revealSquares game ((x,y):ls) = case revealSquare game (x,y) of
                                    Just x  -> x : revealSquares game ls
                                    Nothing -> revealSquares game ls

-- The function execpts a game and an empty location or a number location (must not be a mine). 
-- The function will reveal all the locations that aren't mines given this location
-- Game -> Empty Location -> Game
revealNeighbours :: Game -> Location -> [Location]
revealNeighbours game l 
 | not (isMinelessSquare game l) = [l] -- It's a MINE!
 | otherwise = DL.nub (l : revealNeighbours' game [l] [])

-- Helper function for revealNeighbours
revealNeighbours' :: Game -> [Location] -> [Location] -> [Location] 
revealNeighbours' _ [] _ = [] 
revealNeighbours' game@(_,(w,h)) ((x,y):ls) visited
 | limit && minelessNeighbours game (x,y) && not (elem (x,y) visited)       = (x,y) : revealNeighbours' game surroundings ((x,y):visited)
 | limit && isMinelessSquare game (x,y) && bool && not (elem (x,y) visited) = (x,y) : revealNeighbours' game ls ((x,y):visited)
 | otherwise = revealNeighbours' game ls ((x,y):visited)
    where rv = revealSquare game (x,y)
          bool  = case rv of
                    Just _  -> True 
                    Nothing -> False
          limit = x <= (w-1) && x >= 0 && y <= (h-1) && y >= 0 
          surroundings = (x-1,y-1):(x,y-1):(x+1,y-1):(x-1,y):(x+1,y):(x-1,y+1):(x,y+1):(x+1,y+1):ls
 
 -- mineless square: More preceisely we don't care as long as it's not a mine or hidden
isMinelessSquare :: Game -> Location -> Bool
isMinelessSquare game (x,y) = case revealSquare game (x,y) of
                                Just j -> case j of
                                    Empty    -> True
                                    Hidden   -> False -- Unknown
                                    Mine     -> False
                                    Number _ -> True
                                    Mark     -> True
                                Nothing -> True -- Not a valid coordinate

-- Get a random location on the grid given width and heigt of the grid
randomLocation :: Location -> IO Location
randomLocation (w,h) = do
    myTime <- currTime
    let gen1 = mkStdGen myTime
    let gen2 = mkStdGen (myTime + 1)
    let (x,_) = tg gen1 (w-1)
    let (y,_) = tg gen2 (h-1)
    return $ (x,y)
        where tg gen l = randomR (0,l-1) gen

-- Find the next hidden square to do our solver on. (used in the guessing functionality of the solver)
findNextHidden :: Game -> Maybe Location
findNextHidden game = findNextHidden' game (0,0)

-- Helper function for findNextHidden
findNextHidden' :: Game -> Location -> Maybe Location
findNextHidden' game@(_,(w,h)) (x,y)
 | x >= (w-1) && y >= (h-1) = Nothing
 | x >= (w-1) && y < (h-1)  = findNextHidden' game (0,y+1)
 | otherwise = case revealSquare game (x,y) of
                Just j -> case j of
                    Hidden   -> Just (x,y)
                    _        -> findNextHidden' game (x+1,y)
                Nothing -> Nothing 

{- N is the number we count around. we want to find out the count of a Square around the middle
 - [
 - [m, 3, m],
 - [m, N, m],
 - [m, 3, 1]
 - ]
 -}
countMarksAroundRegion :: Game -> Location -> Int
countMarksAroundRegion game@(b,(w,h)) (x,y) = rg g + rg g1 + rg g2 + rg g3 + rg g4 + rg g5 + rg g6 + rg g7
    where g  = revealSquare game (x-1,y-1) -- up Left
          g1 = revealSquare game (x,y-1)   -- up middle
          g2 = revealSquare game (x+1,y-1) -- up right
          g3 = revealSquare game (x-1,y)   -- middle left 
          g4 = revealSquare game (x+1,y)   -- middle right
          g5 = revealSquare game (x-1,y+1) -- down left
          g6 = revealSquare game (x,y+1)   -- down middle
          g7 = revealSquare game (x+1,y+1) -- down right
          rg g = case g of
                    Just x -> case x of 
                                Mark -> 1
                                _    -> 0
                    Nothing -> 0

-- Despite guessing you'd think it's random, but if I go for randomness, I might as well
-- just go for the first hidden square when enumerating, as random as guessing.
guess :: Game -> Game -> Game
guess pG@(b, (w,h)) lG@(b2,l2) = mycase
    where nh = findNextHidden pG -- Guess work
          jnh = case nh of 
                    Just nh -> nh
                    Nothing -> (0,0)
          (x,y) = jnh
          rv = revealSquare lG (x,y)
          mycase = case rv of
                    Just Empty -> placeLocations pG lG (revealNeighbours lG (x,y))
                    _          -> place pG jnh (b2 !! y !! x)



-- Give the locations except for mark. Given a game and a location find all the locations that aren't a mark useful for the solver
giveLocationsExceptMark :: Game -> Location -> [Location]
giveLocationsExceptMark game@(b, (w,h)) (x,y) = rg g (x-1,y-1) ++ rg g1 (x,y-1) ++ rg g2 (x+1,y-1) ++ rg g3 (x-1,y) ++ rg g4 (x+1,y) ++ rg g5 (x-1,y+1) ++ rg g6 (x,y+1) ++ rg g7 (x+1,y+1)
    where g  = revealSquare game (x-1,y-1) -- up Left
          g1 = revealSquare game (x,y-1)   -- up middle
          g2 = revealSquare game (x+1,y-1) -- up right
          g3 = revealSquare game (x-1,y)   -- middle left 
          g4 = revealSquare game (x+1,y)   -- middle right
          g5 = revealSquare game (x-1,y+1) -- down left
          g6 = revealSquare game (x,y+1)   -- down middle
          g7 = revealSquare game (x+1,y+1) -- down right
          rg g loc = case g of
                    Just x -> case x of 
                                Mark -> []
                                _    -> [loc]
                    Nothing -> [] 

-- anySquare game Mark == True or False -- Is any square on the game board a mark? is the question being answered
anySquare :: Game -> Square -> Bool
anySquare game sq = locateSquares game sq /= []

-- Helper function kind of. This will do the computation which will guarantee in no death to the user. it uses marks by finding locations that are beside the mark to free up
solveLogic :: Location -> Game -> Game -> [Location]
solveLogic (x,y) pG@(b,(w,h)) lG
 | y == h = []
 | x == w = solveLogic (0,y+1) pG lG
 | (b !! y !! x) == Empty = revealNeighbours lG (x,y) ++ solveLogic (x+1,y) pG lG
 | solveisNumber && (solvegiveNumber - countMarksAroundRegion pG (x,y)) == 0 = giveLocationsExceptMark pG (x,y) ++ solveLogic (x+1,y) pG lG
 | otherwise = solveLogic (x+1,y) pG lG
    where solveisNumber = case b !! y !! x of
                        Number n -> True
                        _        -> False
          solvegiveNumber = case b !! y !! x of
                                Number n -> n
                                _        -> 0
-- This is my solver, if the result of calling solveLogic with placeLocations results in the same board (the marks weren't added, therefore make a random guess
-- Note: This solver uses the players help, the player marks obvious spots by flagging it, then the solver will execute the otherwise branch and find the correct places
solver :: Game -> Game -> Game
solver pG lG
 | pG == result = guess pG lG
 | otherwise    = result
    where result = placeLocations pG lG (DL.nub $ solveLogic (0,0) pG lG)

-- Check given a Location if it's surrounded by no mines
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


-- Locate a all Squares in a game 
locateSquares :: Game -> Square -> [Location]
locateSquares g sq = locateSquares' g (0, 0) sq

-- Helper function for locateSquares
locateSquares' :: Game -> Location -> Square -> [Location]
locateSquares' g@(b, (w,h)) (x,y) sq
 | x == w = []
 | y == h = locateSquares' g (x+1,0) sq
 | sq == b !! y !! x = (x,y) : locateSquares' g (x,y+1) sq
 | otherwise         = locateSquares' g (x,y+1) sq

-- Match helps with finding end game condition. if the two lists are the same then pG (mines) == lG (mines) we're done
match :: [Location] -> [Location] -> Bool
match xs ys = DL.sort xs == DL.sort ys

-- Place a random item in the game
placeRandomList :: Game -> [Location] -> Square -> Game
placeRandomList (b, l) [] _ = (b,l)
placeRandomList (b, l) (x:xs) sq = placeRandomList (place (b, l) x sq) xs sq

-- Count the number of squares in the game (this is a duplicate function for reasons (I forgot this existed oops))
countSquares :: Game -> Square -> Int
countSquares (b, _) sq = sum $ map (length . filter (==sq)) b

-- INcrement square (is used with incrementSurroundings for preparing minesweeper board)
incrementSquare :: Game -> Location -> Game
incrementSquare game (x,y) = do
    let rsq = revealSquare game (x,y)
    case rsq of
        Just j -> do
            case j of
               Empty    -> place game (x,y) (Number 1) -- Empty means Number 0 technically
               Mine     -> game
               Hidden   -> game
               Mark     -> game
               Number n -> place game (x,y) (Number (n+1))
        Nothing -> game

-- Increment surroundings of the board given a location (the location of the mine)
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

-- Generate mines randomly for the board give the locations of the mines
-- Lazy evaluation is done here, we want to take the nth unique elmeents from our randomisation so that the mines are unique
generateMines :: Location -> Int -> IO [Location]
generateMines l n = do
    rloc <- randomLocations l
    return $ take n $ DL.nub $ rloc

-- Random locations generation, this will generate mines for us lazily
randomLocations :: Location -> IO [Location]
randomLocations (w,h) = do
    myTime <- currTime
    let gen1 = mkStdGen myTime
    let gen2 = mkStdGen (myTime + 1)
    return $ zip (randomRs (0, w-1) gen1) (randomRs (0, h-1) gen2)

-- small helper function for getting the time as an int
currTime :: IO Int
currTime = do
    time <- getPOSIXTime -- IO POSIXTime
    return (round time)


------------------------------------------
--                                      --
--  Printing Functionality (Debugging)  --
--                                      --
------------------------------------------

-- Helpful debugging tool for printing the board, is used now primarily for debugging threepenny(was also used for the terminal version of this game (I much prefer the terminal tbh)
displayGame :: Game -> IO ()
displayGame = displayGame' 0

-- Helper function for displayGame it will print the board for us nicely with colours and such
displayGame' :: Int -> Game -> IO ()
displayGame' _ ([], (x,y)) = printDefaultColour >> putStr " " >> printList (take x $ repeatIncr (-1)) >> putStrLn (show x ++ "x" ++ show y)
displayGame' i ((l:ls), (x,y)) = printDefaultColour >> putStr "[" >>  displayRow l  >> putStr "]" >> putStrLn (" " ++ (show i)) >> printDefaultColour >> displayGame' (i+1) (ls,(x,y))

-- Function used for printing the board (it helps with incrementing the index for visualisation for the board)
repeatIncr :: (Num b, Enum b) => b -> [b]
repeatIncr x = map (+1) [x..] 

-- Print a list (the list is row items
printList :: Show a => [a] -> IO ()
printList []  = return ()
printList [x] = putStrLn (show x) 
printList (x:xs) = putStr ((show x) ++ ", ") >> printList xs

-- default colour gray
printDefaultColour :: IO ()
printDefaultColour = putStr "\ESC[90m"

-- Displays a single row of the board (takes in a row
displayRow :: Row -> IO ()
displayRow []  = printDefaultColour
displayRow [Empty]    = putStr "\ESC[36mE" >> printDefaultColour 
displayRow [Mine]     = putStr "\ESC[31mM" >> printDefaultColour
displayRow [Hidden]   = putStr "\ESC[35mH" >> printDefaultColour
displayRow [Mark]     = putStr "\ESC[34mX" >> printDefaultColour
displayRow [Number n] = putStr ("\ESC[32m" ++ (show n)) >> printDefaultColour
displayRow ((Empty):xs)    = putStr "\ESC[36mE, " >> displayRow xs
displayRow ((Mine):xs)     = putStr "\ESC[31mM, " >> displayRow xs
displayRow ((Hidden):xs)   = putStr "\ESC[35mH, " >> displayRow xs
displayRow ((Mark):xs)     = putStr "\ESC[34mX, " >> displayRow xs
displayRow ((Number n):xs) = putStr ("\ESC[32m" ++ (show n ++ ", ")) >> displayRow xs

------------------------------------------
--                                      --
--      End of Printing (Debugging)     --
--                                      --
------------------------------------------

-- Gamestate is the logic game, the game the player doesn't actually see, the full board (board is empty for now)
-- This function is used in game function where it loads in the mines and so on.
gameState :: (Int,Int) -> Game
gameState (x,y) = setup x y Empty

-- This is the game the player sees, notice how the board is hidden using the Hidden constructor
playerGameState :: (Int,Int) -> Game
playerGameState (x,y) = setup x y Hidden

playerGame :: (Int,Int) -> IO Game
playerGame size = do
    --putStrLn "Player Game State"
    return $ playerGameState size

-- This is the logic game, the player doesn't see this game, it's the full board showing each location of everything
game :: (Int,Int) -> Int -> IO Game
game size numMines = do
    --putStrLn "Game State"
    let (_, (x, y)) = gameState size 
    mines <- generateMines (x,y) numMines 
    let mineGame   = placeRandomList (gameState size) (mines) Mine
    let numberGame = placeNumbers mineGame mines
    return $ numberGame

-- This game loop is old, don't look at this, it's only here for debugging purposes
-- This verion of minsweeper doesn't have the solver, everything else is p much the same
-- It's a nice terminal version of the game, all it requires is (pG <- playerGame (x,y)) and (lG <- game (x,y)) to be passed in to play
gameLoop :: Game -> Game -> IO ()
gameLoop pG lG = do
    displayGame pG
    if match ((locateSquares pG Mark) ++ (locateSquares pG Hidden)) (locateSquares lG Mine) 
        then do 
            putStrLn "You win"
            displayGame lG
        else do
            putStrLn "Mark or select square (m/s)?: "
            ms <- getLine
            putStrLn "Give x coordinate "
            x <- getLine
            putStrLn "Give y coordinate "
            y <- getLine
            let xInt = read x :: Int
            let yInt = read y :: Int
            let sq = revealSquare lG (xInt, yInt)
            case sq of
                Just a  -> do
                    if ms == "m"
                        then do
                            let psq = revealSquare pG (xInt, yInt)
                            if psq == Just Mark
                                then do
                                    putStrLn "You have marked the square"
                                    gameLoop (place pG (xInt, yInt) Hidden) lG
                                else do
                                    putStrLn "You have marked the square"
                                    gameLoop (place pG (xInt,yInt) Mark) lG
                        else do
                            putStrLn ("The Square is " ++ (show a))
                            case a of
                                Mine -> do
                                    displayGame lG
                                    putStrLn "Game Over"
                                _    -> do
                                    let updatedGame = (place pG (xInt,yInt) a)
                                    let rN = revealNeighbours lG (xInt, yInt)
                                    let newestGame = placeLocations updatedGame lG rN
                                    gameLoop newestGame lG
                Nothing -> do
                    putStrLn "(x,y) Given was invalid, try again"
                    gameLoop pG lG 
