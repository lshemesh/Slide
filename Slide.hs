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

printGameState :: Board -> Board -> IO ()
printGameState start end = do
  putStrLn $ show end
  putStrLn $ show start
  putStr $ "Available Moves " ++ concatMap (show . swap) (getAvailableMoves start) ++ ":"

play :: Level -> IO ()
play (start, end) = do
  resetScreen
  printGameState start end
  coords <- getInputCoords
  next <- return $ move (swap coords) start 
  if next == end 
    then putStrLn "You won!"
    else play (next, end)
    
main = play level2
