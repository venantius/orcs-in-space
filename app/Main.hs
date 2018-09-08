module Main where

import Brick (customMain)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Game (Tick(..), app, initGame)
import qualified Graphics.Vty as V

main :: IO ()
main = do
    chan <- newBChan 10
    forkIO $
        forever $ do
            writeBChan chan Tick
            threadDelay 400000 -- how fast the game moves
    g <- initGame
    void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g
