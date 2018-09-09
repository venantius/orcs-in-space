{-# LANGUAGE OverloadedStrings #-}

module UI where

import Game (Game(..), occupiedPositions, recursiveUnitAIMove, turn)
import Grid.Coord (Coord(..))
import Structures.Wall
    ( Structure(..)
    , StructureType(..)
    , StructureType
    , position
    )
import Units.Core (Direction(..), Race(..), Unit(..), position)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Control.Monad.IO.Class (liftIO)

-- I might ditch both of these later
height :: Int
height = 40

width :: Int
width = 80

-- Unused; Brick recommends just using () when unused.
type Name = ()

data Tick =
    Tick

app :: App Game Tick Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return
        , appAttrMap = const theMap
        }

-- Handling events
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = step g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ turn West g
handleEvent g _ = continue g

-- Move all units according to their AI
step :: Game -> EventM Name (Next Game)
step g = do
    u <- liftIO $ recursiveUnitAIMove (units g) (occupiedPositions g)
    continue $ g {units = u}

-- Drawing
drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g = hLimit 22 $ vBox [drawScore "This is placeholder"]

drawScore :: [Char] -> Widget Name
drawScore n =
    withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Score") $ C.hCenter $ padAll 1 $ str n

drawGrid :: Game -> Widget Name
drawGrid g =
    withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Map") $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height - 1,height - 2 .. 0]]
    cellsInRow y = [drawCoord (Coord x y) | x <- [0 .. width - 1]]
    drawCoord = drawCell . cellAt g

cellAt :: Game -> Coord -> Cell
cellAt g c
    | elem c orcs = OrcCell
    | elem c spaceMarines = SpaceMarineCell
    | elem c walls = WallCell
    | otherwise = Empty
  where
    walls =
        concat $
        map Structures.Wall.position $
        filter ((== Wall) . structureType) $ structures g
    orcs = map Units.Core.position $ filter ((== Orc) . race) $ units g
    spaceMarines =
        map Units.Core.position $ filter ((== SpaceMarine) . race) $ units g

data Cell
    = OrcCell
    | SpaceMarineCell
    | WallCell
    | Empty

drawCell :: Cell -> Widget Name
drawCell Empty = withAttr emptyAttr $ str " "
drawCell OrcCell = withAttr orcAttr $ str "O"
drawCell SpaceMarineCell = withAttr spaceMarineAttr $ str "S"
drawCell WallCell = withAttr wallAttr $ str "X"

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (orcAttr, fg V.green)
        , (spaceMarineAttr, fg V.white)
        , (wallAttr, bg V.white)
        ]

orcAttr :: AttrName
orcAttr = "orcAttr"

spaceMarineAttr :: AttrName
spaceMarineAttr = "spaceMarineAttr"

wallAttr :: AttrName
wallAttr = "wallAttr"

emptyAttr :: AttrName
emptyAttr = "emptyAttr"
