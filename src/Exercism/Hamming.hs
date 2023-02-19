module Exercism.Hamming (distance) where

-- https://exercism.org/tracks/haskell/exercises/hamming
-- https://github.com/exercism/haskell/tree/27ddf01e23113961c03db7ebc905369c2b1fad04/exercises/practice/hamming

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance (x:xs) (y:ys)
  | x == y     = distance xs ys
  | otherwise  = (+1) <$> distance xs ys
distance _ _   = Nothing
