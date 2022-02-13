module Utils where

import Data.List (elemIndex)

position :: Eq a => a -> [a] -> Int
position i xs =
    case i `elemIndex` xs of
       Just n  -> n
       Nothing -> -1

contain :: (Eq a) => [a] -> a -> Bool
contain [] _ = False
contain (x:xs) a
  | x == a    = True
  | otherwise = contain xs a

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | contain xs x  = unique xs
  | otherwise = x : unique xs

isEmpty :: [Int] -> Bool
isEmpty [] = True
isEmpty struct = False

len :: [Int] -> Int
len [] = 0
len struct = length struct


