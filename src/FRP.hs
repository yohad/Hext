module FRP where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad (forever)
import Control.Monad.State (execState)
import Mechanism

makeNetworkDescription :: AddHandler Char -> MomentIO ()
makeNetworkDescription addKeyEvent =  do
    eKey <- fromAddHandler addKeyEvent
    let 
        eInsertChar = execInsert <$> eKey
        bState = accumB initialState eInsertChar
    eStateChanged <- changes bState
    reactimate' $ fmap (\state -> putStrLn ("Text: " ++ getText state ++ "; Pointer: " ++ getPointer state))
                  <$> eStateChanged

main :: IO ()
main = do
    (addKeyEvent, fireEvent) <- newAddHandler
    network <- compile $ makeNetworkDescription addKeyEvent
    actuate network
    forever $ getChar >>= fireEvent
