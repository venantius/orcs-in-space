module Game where

import Brick
import Grid.Coord (Coord(..))
import Structures.Wall
    ( Structure(..)
    , StructureType
    , StructureType(..)
    , buildFort
    )
import Units.Core (Direction(..), Race(..), Unit, move, position, race)
import Units.Orc (createOrc, moveRandomly)
import Units.SpaceMarine (createSpaceMarine)

import System.Random (StdGen(..), newStdGen)

data Game = Game
    { seed :: StdGen
    , units :: [Unit]
    , structures :: [Structure]
    } deriving (Show)

initGame :: IO Game
initGame = do
    seed <- newStdGen
    return
        Game
            { seed = seed
            , units = [createOrc, createSpaceMarine]
            , structures = buildFort (Coord 5 10) (Coord 15 15)
            }

occupiedPositions :: Game -> [Coord]
occupiedPositions g = unitPositions ++ structurePositions
  where
    unitPositions = map Units.Core.position $ units g
    structurePositions = concat $ map Structures.Wall.position $ structures g

-- Update each unit with a move, decrement i, recurse
recursiveUnitAIMove :: [Unit] -> [Coord] -> IO [Unit]
recursiveUnitAIMove [] _ = return []
recursiveUnitAIMove [u] pos = do
    seed <- newStdGen
    let movedU = moveRandomly u seed pos -- update the first unit with a random move
    return [movedU]
recursiveUnitAIMove (u:us) pos = do
    seed <- newStdGen
    let movedU = moveRandomly u seed pos -- update the first unit with a random move
    movedUs <- recursiveUnitAIMove us pos -- update the remaining units with a new seed
    return $ movedUs ++ [movedU]

turn :: Units.Core.Direction -> Game -> Game
turn d g = g {units = [move d u | u <- units g]}
