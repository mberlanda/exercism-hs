module Exercism.Yacht (yacht, Category(..)) where

-- https://exercism.org/tracks/haskell/exercises/yacht
-- https://github.com/exercism/haskell/tree/27ddf01e23113961c03db7ebc905369c2b1fad04/exercises/practice/yacht

import Data.Function ( on )
import Data.List ( group, sort, sortBy )

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht
              deriving (Enum)

type RawDice = [Int]
type ResultShape = [[Int]]
type TargetShape = [Int]

yacht :: Category -> RawDice -> Int
yacht Choice           = sum
yacht FullHouse        = fullHouse
yacht FourOfAKind      = fourOfAKind
yacht LittleStraight   = solveBySort [1, 2, 3, 4, 5]
yacht BigStraight      = solveBySort [2, 3, 4, 5, 6]
yacht Yacht            = yacht'
yacht number           = numbers ( 1 + fromEnum number)

numbers :: Int -> RawDice -> Int
numbers int lst = int * length (filter (== int) lst)

solveBySort :: TargetShape -> RawDice -> Int
solveBySort target dice = 30 * fromEnum (target == sort dice)

fullHouse :: RawDice -> Int
fullHouse dice = sum dice * fromEnum (shapeEq [3, 2] dice)

fourOfAKind :: RawDice -> Int
fourOfAKind dice = sum fours
   where
      headShape = head . shape
      fours
       | shapeEq [4, 1] dice = headShape dice
       | shapeEq [5]  dice   = tail $ headShape dice
       | otherwise           = [] 

yacht' :: RawDice -> Int
yacht' dice = 50 * fromEnum (shapeEq [5] dice)

shape :: RawDice -> ResultShape
shape = sortBy (flip compare `on` length) . group . sort

countShape :: ResultShape -> TargetShape
countShape = map length

shapeEq :: TargetShape -> RawDice -> Bool
shapeEq target dice = target == countShape (shape dice)
