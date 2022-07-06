module Main where

import Lib

gameMenu :: User -> Int -> IO ()
gameMenu user round = do
    putStrLn "\nWant to play?"
    putStrLn "1. Yes"
    putStrLn "2. No (exit)"
    input1 <- getLine 
    let choice = (read input1 :: Int)  -- how to make this safe? i.e., 'a' doesnt work
    -- GOOSE
    -- try/catch in haskell https://stackoverflow.com/questions/6009384/exception-handling-in-haskell
    -- GOOSE
    -- how to deal with recursion/while loop
    if choice == 1
        then do
            startRound user round
            gameMenu user (round + 1)
    else putStrLn "Thanks for playing!"
            
--
-- MAIN
--
main = do
    let user = User { userName="Daniel", userRoster=rosterEmpty}
    gameMenu user 1