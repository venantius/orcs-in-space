module Grid.Coord where

data Coord
    = Coord { x :: Int
            , y :: Int }
    | Int Int
    deriving (Show, Eq)
