module Units.SpaceMarine where

import Grid.Coord
import Units.Core

createSpaceMarine :: Unit
createSpaceMarine =
    Unit
        { race = SpaceMarine
        , totalHitPoints = 50
        , currentHitPoints = 50
        , weapon = Weapon Bolter 10
        , position = Coord 15 15
        }
