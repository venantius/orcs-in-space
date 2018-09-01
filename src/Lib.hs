module Lib
    ( main
    ) where

import Brick

ui :: Widget ()
ui = str "Hello, World!"

main :: IO ()
main = simpleMain ui
