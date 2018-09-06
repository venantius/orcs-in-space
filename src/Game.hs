{-# LANGUAGE OverloadedStrings #-}

module Game
    ( Game
    , Tick(..)
    , app
    , initGame
    ) where

import Brick
import Grid.Coord (Coord(..))
import Units.Core (Direction(..), Unit, move, position)
import Units.Orc (createOrc)

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

-- I might ditch both of these later
height :: Int
height = 20

width :: Int
width = 20

data Tick =
    Tick

-- Unused; Brick recommends just using () when unused.
type Name = ()

data Game = Game
    { units :: [Unit]
    } deriving (Show)

initGame :: IO Game
initGame = do
    return Game {units = [createOrc]}

-- TODO
step :: Game -> Game
step g = g

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
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ turn West g
handleEvent g _ = continue g

turn :: Units.Core.Direction -> Game -> Game
turn d g = g {units = [move d u | u <- units g]}

-- Drawing
drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11 $ vBox [drawScore 30]

drawScore :: Int -> Widget Name
drawScore n =
    withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Score") $ C.hCenter $ padAll 1 $ str $ show n

drawGrid :: Game -> Widget Name
drawGrid g =
    withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Map") $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height - 1,height - 2 .. 0]]
    cellsInRow y = [drawCoord (Coord x y) | x <- [0 .. width - 1]]
    drawCoord = drawCell . cellAt g

cellAt :: Game -> Coord -> Cell
cellAt g c
    | elem c $ map position $ units g = Unit
    | otherwise = Empty

data Cell
    = Unit
    | Empty

drawCell :: Cell -> Widget Name
drawCell Unit = withAttr unitAttr $ str "O"
drawCell Empty = withAttr emptyAttr $ str " "

theMap :: AttrMap
theMap = attrMap V.defAttr [(unitAttr, fg V.green)]

unitAttr :: AttrName
unitAttr = "unitAttr"

emptyAttr :: AttrName
emptyAttr = "emptyAttr"
