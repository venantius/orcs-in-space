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

moveRandomly :: Unit -> StdGen -> [Coord] -> Unit
moveRandomly u rng occupiedPositions = do
    let i = fst $ randomR (0, 3) rng
    let d = [North, South, East, West] !! i
    move d u
