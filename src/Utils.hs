module Utils where

import Data.List (foldl')

swapInList :: Int -> Int -> [a] -> [a]
swapInList    from   to     xs   = foldl' fn [] [0..(length xs)-1]
  where fn xs' index | index == from = xs' ++ [(xs !! to)]
                     | index == to   = xs' ++ [(xs !! from)]
                     | otherwise     = xs' ++ [(xs !! index)]

groupsIn :: Int -> [a] -> [[a]]
groupsIn n [] = []
groupsIn n xs = first : groupsIn n rest
  where (first, rest) = splitAt n xs

groupsInThree :: [a] -> [[a]]
groupsInThree = groupsIn 3


