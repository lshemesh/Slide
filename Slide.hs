module Slide where

import Data.List (find, foldl')
import Data.Tuple (swap)

data Tile = Green | Blue | Red | Blank deriving (Eq)

newtype Board = Board [Tile] deriving (Eq)

instance Show Board where
  show (Board boardList) = unlines (map show (groupsInThree boardList))

instance Show Tile where
  show Green = "\x1b[32m*\x1b[0m"
  show Red   = "\x1b[31m*\x1b[0m"
  show Blue  = "\x1b[34m*\x1b[0m"
  show _     = " "

type Level = (Board, Board)

type Coords = (Int, Int)

level1 :: Level
level1 = 
  (
  Board [ Green,  Red,    Blank,
          Red,    Blue,   Green,
          Blue,   Green,  Red   ]
  ,
  Board [ Red,    Blue,   Green,
          Red,    Green,  Blank,
          Green,  Red,    Blue  ]
  )

groupsIn :: Int -> [a] -> [[a]]
groupsIn n [] = []
groupsIn n xs = first : groupsIn n rest
  where (first, rest) = splitAt n xs

groupsInThree :: [a] -> [[a]]
groupsInThree = groupsIn 3

getTileAt :: Coords -> Board -> Maybe Tile
getTileAt coords (Board boardList) 
  | index < 0                       = Nothing
  | index > (length boardList) - 1  = Nothing
  | otherwise                       = Just (boardList !! index)
  where index = getIndex coords

getNextMove :: Coords -> Board -> Maybe (Int,Int)
getNextMove (x,y) board = find ((flip isBlank) board) (getNeighbors (x,y))

getNeighbors :: Coords -> [Coords]
getNeighbors (x,y) = [(x,y-1), (x+1,y), (x,y+1), (x-1,y)]

isBlank :: Coords -> Board -> Bool
isBlank (x,y) board = maybe False (==Blank) $ getTileAt (x,y) board

getIndex :: Coords -> Int
getIndex (x,y) = x * 3 + y

mkMove :: Coords -> Coords -> Board -> Board
mkMove (x,y) (x',y') (Board boardList) = Board (swapInList from to boardList)
  where from = getIndex (x,y)
        to = getIndex (x',y')

swapInList :: Int -> Int -> [a] -> [a]
swapInList    from   to     xs   = foldl' fn [] [0..(length xs)-1]
  where fn xs' index | index == from = xs' ++ [(xs !! to)]
                     | index == to   = xs' ++ [(xs !! from)]
                     | otherwise     = xs' ++ [(xs !! index)]

move :: Coords -> Board -> Board
move from board = 
  case getNextMove from board of
    Just to -> mkMove from to board
    Nothing -> board
        
getInputCoords :: IO Coords
getInputCoords = do
  coords <- getLine 
  return $ read coords

play :: Level -> IO ()
play (start, end) = do
  putStrLn $ show end
  putStrLn $ show start
  putStr "Pick a tile to move: "
  coords <- getInputCoords
  next <- return $ move (swap coords) start 
  if next == end 
    then return ()
    else play (next, end)
    
main = play level1
