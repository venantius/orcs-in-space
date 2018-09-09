module Structures.Wall where

import Data.List

import Grid.Coord

data StructureType =
    Wall
    deriving (Show, Eq)

data Structure = Structure
    { totalHitPoints :: Int
    , currentHitPoints :: Int
    , structureType :: StructureType
    , position :: [Coord]
    } deriving (Show)

buildFort :: Coord -> Coord -> [Structure]
buildFort tl br = [s]
  where
    tlX = x tl
    tlY = y tl
    brX = x br
    brY = y br
    horizontalWalls =
        [Coord x tlY | x <- [tlX .. brX]] ++ [Coord x brY | x <- [tlX .. brX]]
    verticalWalls =
        [Coord tlX y | y <- [tlY .. brY]] ++ [Coord brX y | y <- [tlY .. brY]]
    s =
        Structure
            { totalHitPoints = 50
            , currentHitPoints = 50
            , structureType = Wall
            , position = nub $ horizontalWalls ++ verticalWalls
            }
