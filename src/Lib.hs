module Lib
    ( someFunc,
      mainRoutine
    ) where

import qualified FRP
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

mainRoutine :: IO ()
mainRoutine = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    FRP.main
