module Board 
  ( Tile (..)
  , Board (Board)
  , Coords
  , getAvailableMoves
  , move
  , getBlankCoords
  , getNeighbors
  ) where

import Data.List (find, findIndex, concatMap)
import Data.Maybe (isJust)
import Data.Tuple (swap)

import Utils (groupsInThree, swapInList)

data Tile = G | B | R | Blank deriving (Eq)

newtype Board = Board [Tile] deriving (Eq)

instance Show Board where
  show (Board boardList) = unlines (map (concatMap show) $ groupsInThree boardList)

instance Show Tile where
  show G = "\x1b[32m*\x1b[0m"
  show R = "\x1b[31m*\x1b[0m"
  show B = "\x1b[34m*\x1b[0m"
  show _ = " "

type Coords = (Int, Int)

getTileAt :: Board -> Coords -> Maybe Tile
getTileAt (Board boardList) coords
  | index < 0                       = Nothing
  | index > (length boardList) - 1  = Nothing
  | otherwise                       = Just (boardList !! index)
  where index = getIndex coords

getNextMove :: Board -> Coords -> Maybe Coords
getNextMove board coords = find (isBlank board) $ getNeighbors coords

getNeighbors :: Coords -> [Coords]
getNeighbors (x,y) = filter good [(x,y-1), (x+1,y), (x,y+1), (x-1,y)]
  where good (x',y') = x' >= 0  && x' < 3 && y' >= 0 && y' < 3

isBlank :: Board -> Coords -> Bool
isBlank board (x,y) = maybe False (==Blank) $ getTileAt board (x,y)

getBlankCoords :: Board -> Maybe Coords
getBlankCoords (Board boardList) = findIndex (==Blank) boardList >>= (\x -> return $ getCoords x)

getAvailableMoves :: Board -> [Coords]
getAvailableMoves board =
  case getBlankCoords board of
    Just blankCoords -> filter (isJust . (getTileAt board)) (getNeighbors blankCoords)
    Nothing -> []

getIndex :: Coords -> Int
getIndex (y,x) = x * 3 + y

getCoords :: Int -> Coords
getCoords x = swap $ x `divMod` 3

mkMove :: Board -> Coords -> Coords -> Board
mkMove (Board boardList) from to = Board (swapInList fromIndex toIndex boardList)
  where fromIndex = getIndex from
        toIndex   = getIndex to

move :: Board -> Coords -> Board
move board from = 
  case getNextMove board from of
    Just to -> mkMove board from to
    Nothing -> board
