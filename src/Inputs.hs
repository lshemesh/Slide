module Inputs (getInput) where

import qualified Data.Map as M

import Board (Coords, Board, getBlankCoords, getNeighbors)

getKeyMoveMapping :: Board -> M.Map Char Coords
getKeyMoveMapping board = 
  case getBlankCoords board of
    Just coords -> buildMap coords
    Nothing -> M.empty
  where 
    buildMap (x,y) = M.fromList $ map (mapF (x,y)) (getNeighbors (x,y))
    mapF (x,y) (x',y') 
      | x' > x = ('a', (x',y'))
      | x' < x = ('d', (x',y'))
      | y' > y = ('w', (x',y'))
      | y' < y = ('s', (x',y'))

getInput :: Board -> IO (Maybe Coords)
getInput board = do
  key <- getChar
  keyMap <- return $ getKeyMoveMapping board
  return $ M.lookup key keyMap
