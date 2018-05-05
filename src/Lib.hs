module Lib
    ( someFunc,
      mainRoutine
    ) where

import qualified FRP

someFunc :: IO ()
someFunc = putStrLn "someFunc"

mainRoutine :: IO ()
mainRoutine = FRP.main
