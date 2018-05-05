module Mechanism where

import Control.Monad.State

data Content = Content {
    getText :: String,
    getPointer :: Int
                       }

empty :: Content
empty = Content "" 0

initialState = Content "" 0

fetchText :: State Content String
fetchText = get >>= (return . getText)

fetchPointer :: State Content Int
fetchPointer = get >>= (return . getPointer)

setText :: String -> State Content ()
setText text = do
    pos <- fetchPointer
    put $ Content text pos

addToPointer :: Int -> State Content ()
addToPointer n = do
    text <- fetchText
    pos <- fetchPointer
    put $ Content text (pos + n)

insert :: Char -> State Content ()
insert c = do
    text <- fetchText
    pos <- fetchPointer
    let (before, after) = splitAt pos text in setText $ before ++ c : after
    addToPointer 1

execInsert :: Content -> Content
execInsert = execState . insert

insertMany :: String -> State Content ()
insertMany = mapM_ insert
        
