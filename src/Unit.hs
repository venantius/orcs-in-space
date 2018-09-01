module Unit where

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

data Orc

data SpaceMarine

data Race
    = Orc
    | SpaceMarine
    deriving (Show)

data Unit = Unit
    { race :: Race
    , totalHitPoints :: Int
    , currentHitPoints :: Int
    , weapon :: Weapon
    } deriving (Show)

shoot :: Unit -> Unit -> Unit
shoot (Unit _ _ _ (Weapon t d)) (Unit r thp c w) = (Unit r thp (c - d) w)
