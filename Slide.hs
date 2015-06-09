module Slide where

import Data.Tuple (swap)
import System.Console.ANSI

import Board (Coords, Board (..), Tile (..), getAvailableMoves, move)
import Levels
        
getInputCoords :: IO Coords
getInputCoords = do
  coords <- getLine 
  return $ read coords

resetScreen :: IO ()
resetScreen = setCursorPosition 0 0 >> clearScreen

printBoards :: Board -> Board -> IO ()
printBoards start end = do
  putStr $ show end
  putStrLn "---"
  putStrLn $ show start

printAvailableMoves :: Board -> IO ()
printAvailableMoves start = do
  putStr $ "Available Moves " ++ concatMap (show . swap) (getAvailableMoves start) ++ ":"

play :: Level -> IO ()
play (start, end) = do
  resetScreen
  printBoards start end
  printAvailableMoves start
  coords <- getInputCoords
  next <- return $ move (swap coords) start 
  if next == end 
    then do
      resetScreen
      printBoards next end
      putStrLn "YOU DID IT!"
    else play (next, end)
    
main = play level2
