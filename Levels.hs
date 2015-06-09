module Levels where

import Board (Board (..), Tile (..))

type Level = (Board, Board)

level1 :: Level
level1 = 
  (
  Board [ G, R, Blank,
          R, B, G,
          B, G, R ]
  ,
  Board [ R, B, G,
          R, G, Blank,
          G, R, B ]
  )

level2 :: Level
level2 = 
  (
  Board [ G, R, Blank,
          R, B, G,
          B, G, R ]
  ,
  Board [ G, Blank, R,
          R, B, G,
          B, G, R ]
  )

