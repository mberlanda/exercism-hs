module Exercism.CollatzConjecture (collatz) where

-- https://exercism.org/tracks/haskell/exercises/collatz-conjecture
-- https://github.com/exercism/haskell/blob/77a68cb395e19b4b2082163a5190fafc815082da/exercises/practice/collatz-conjecture/src/CollatzConjecture.hs#L3

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz x | x <= 0    = Nothing
          | even x    = (+1) <$> collatz (x `div` 2)
          | otherwise = (+1) <$> collatz (3 * x + 1)
