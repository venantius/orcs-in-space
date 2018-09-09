module Units.Orc where

import System.Random

import Grid.Coord
import Units.Core

createOrc :: Unit
createOrc =
    Unit
        { race = Orc
        , totalHitPoints = 50
        , currentHitPoints = 50
        , weapon = Weapon Beam 10
        , position = Coord 10 10
        }

shiftCoord :: Int -> Int -> Coord -> Coord
shiftCoord xShift yShift c = Coord ((x c) + xShift) ((y c) + yShift)

adjacentCoords :: Unit -> [Coord]
adjacentCoords u = [northPos, eastPos, southPos, westPos]
  where
    pos = position u
    northPos = shiftCoord 0 1 pos
    eastPos = shiftCoord 1 0 pos
    southPos = shiftCoord 0 (-1) pos
    westPos = shiftCoord (-1) 0 pos

isElem :: [Coord] -> Coord -> Bool
isElem coll e = elem e coll

moveRandomly :: Unit -> StdGen -> [Coord] -> Unit
moveRandomly u rng occupiedPositions = do
    let i = fst $ randomR (0, (length adjacencies) - 1) rng
    let newPos = adjacencies !! i
    u {position = newPos}
  where
    adjacencies = filter (not . isElem occupiedPositions) $ adjacentCoords u
