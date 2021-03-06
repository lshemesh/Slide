module Slide where

import System.Console.ANSI
import System.IO

import Board (Board, move)
import Levels
import Inputs (getInput)

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
