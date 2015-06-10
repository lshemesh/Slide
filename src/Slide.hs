module Slide where

import System.Console.ANSI
import System.IO
import qualified Data.Map as M

import Board (Coords, Board (..), Tile (..), getAvailableMoves, move, getBlankCoords, getNeighbors)
import Levels

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

resetScreen :: IO ()
resetScreen = setCursorPosition 0 0 >> clearScreen

printBoards :: Board -> Board -> IO ()
printBoards start end = do
  putStr $ show end
  putStrLn ""
  putStrLn $ show start

play :: Level -> IO ()
play (start, end) = do
  resetScreen
  printBoards start end
  coords <- getInput start
  next <- return $ maybe start (move start) coords
  if next == end 
    then do
      resetScreen
      printBoards next end
      putStrLn "YOU DID IT!"
    else play (next, end)
    
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  play level1
