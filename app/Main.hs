module Main where

import Lib

gameMenu :: Int -> IO ()
gameMenu round = do
    putStrLn "Want to play?"
    putStrLn "1. Yes"
    putStrLn "2. No (exit)"
    input1 <- getLine 
    let choice = (read input1 :: Int)
    if choice == 1
        then do
            startRound round
            gameMenu (round + 1)
    else putStrLn "Thanks for playing!"
            
--
-- MAIN
--
main = do
    gameMenu 1