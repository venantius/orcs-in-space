module Units.Core where

import Grid.Coord

data Bolter

data Beam

data WeaponType
    = Bolter
    | Beam
    deriving (Show)

data Weapon = Weapon
    { weaponType :: WeaponType
    , damage :: Int
    } deriving (Show)

data Race
    = Orc
    | SpaceMarine
    deriving (Show, Eq)

data Unit = Unit
    { race :: Race
    , totalHitPoints :: Int
    , currentHitPoints :: Int
    , weapon :: Weapon
    , position :: Coord
    } deriving (Show)

shoot :: Unit -> Unit -> Unit
shoot (Unit _ _ _ (Weapon t d) _) (Unit r thp c w pos) =
    (Unit r thp (c - d) w pos)

data Direction
    = North
    | East
    | South
    | West
    deriving (Eq, Show)

move :: Direction -> Unit -> Unit
move d u
    | d == North = movePosition 0 1 u
    | d == East = movePosition 1 0 u
    | d == South = movePosition 0 (-1) u
    | d == West = movePosition (-1) 0 u

movePosition :: Int -> Int -> Unit -> Unit
movePosition xShift yShift u =
    u {position = Coord (xPos + xShift) (yPos + yShift)}
  where
    xPos = x $ position u
    yPos = y $ position u
