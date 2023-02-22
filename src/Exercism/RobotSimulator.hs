module Exercism.RobotSimulator
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

-- https://exercism.org/tracks/haskell/exercises/robot-simulator
-- https://github.com/exercism/haskell/blob/2ef2c03b1ef83c2c11c493c594a5dfae4aa39e5c/exercises/practice/robot-simulator/src/Robot.hs

data Bearing = North
             | East
             | South
             | West
             deriving (Bounded, Enum, Eq, Show)

type Coordinates = (Integer, Integer)

data Robot = Robot { coordinates :: Coordinates, bearing :: Bearing } deriving (Eq, Show)

mkRobot :: Bearing -> Coordinates -> Robot
mkRobot dir coord = Robot { bearing = dir, coordinates = coord }

move :: Robot -> String -> Robot
move = foldl (flip move')

move' :: Char -> Robot -> Robot
move' 'A' = advance
move' 'L' = turn toLeft
move' 'R' = turn toRight
move' i   = error ("Unsupported move " ++ [i])

advance :: Robot -> Robot
advance robot = mkRobot dir coord
  where
    dir   = bearing robot
    coord = advance' dir (coordinates robot)

advance' :: Bearing -> Coordinates -> Coordinates
advance' East  = \(x, y) -> (x + 1, y)
advance' North = \(x, y) -> (x, y + 1)
advance' West  = \(x, y) -> (x - 1, y)
advance' South = \(x, y) -> (x, y - 1)

toRight :: Bearing -> Bearing
toRight b | b == maxBound = minBound
          | otherwise     = succ b

toLeft :: Bearing -> Bearing
toLeft b | b == minBound = maxBound
         | otherwise     = pred b

turn :: (Bearing -> Bearing) -> Robot -> Robot
turn f robot = mkRobot (f $ bearing robot) (coordinates robot)
