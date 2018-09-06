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

moveRandomly :: Unit -> Unit
moveRandomly u = do
    i <- getStdRandom $ randomR (0, 3)
    let dir = [North, South, East, West] !! i
    move dir u
