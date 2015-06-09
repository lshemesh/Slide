module Board 
  ( Tile (..)
  , Board (Board)
  , Coords
  , getAvailableMoves
  , move
  ) where

import Data.List (find, findIndex, concatMap)
import Data.Maybe (isJust)
import Utils (groupsInThree, swapInList)

data Tile = G | B | R | Blank deriving (Eq)

newtype Board = Board [Tile] deriving (Eq)

instance Show Board where
  show (Board boardList) = unlines (map show (groupsInThree boardList))

instance Show Tile where
  show G = "\x1b[32m*\x1b[0m"
  show R = "\x1b[31m*\x1b[0m"
  show B = "\x1b[34m*\x1b[0m"
  show _ = " "

type Coords = (Int, Int)

getTileAt :: Coords -> Board -> Maybe Tile
getTileAt coords (Board boardList) 
  | index < 0                       = Nothing
  | index > (length boardList) - 1  = Nothing
  | otherwise                       = Just (boardList !! index)
  where index = getIndex coords

getNextMove :: Coords -> Board -> Maybe Coords
getNextMove (x,y) board = find ((flip isBlank) board) (getNeighbors (x,y))

getNeighbors :: Coords -> [Coords]
getNeighbors (x,y) = [(x,y-1), (x+1,y), (x,y+1), (x-1,y)]

isBlank :: Coords -> Board -> Bool
isBlank (x,y) board = maybe False (==Blank) $ getTileAt (x,y) board

getBlankCoords :: Board -> Maybe Coords
getBlankCoords (Board boardList) = findIndex (==Blank) boardList >>= (\x -> return $ getCoords x)

getAvailableMoves :: Board -> [Coords]
getAvailableMoves board =
  case getBlankCoords board of
    Just blankCoords -> filter (isJust . ((flip getTileAt) board)) (getNeighbors blankCoords)
    Nothing -> []

getIndex :: Coords -> Int
getIndex (x,y) = x * 3 + y

getCoords :: Int -> Coords
getCoords x = x `divMod` 3

mkMove :: Coords -> Coords -> Board -> Board
mkMove (x,y) (x',y') (Board boardList) = Board (swapInList from to boardList)
  where from = getIndex (x,y)
        to   = getIndex (x',y')

move :: Coords -> Board -> Board
move from board = 
  case getNextMove from board of
    Just to -> mkMove from to board
    Nothing -> board