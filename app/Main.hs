module Main where

import Lib

gameMenu :: ()
gameMenu = 
    putStrLn "Want to play?"
    putStrLn "1. Yes"
    putStrLn "2. No (exit)"
    choice <- getLine :: Integer
    if choice == 1
        then startRound
        else putStrLn "Thanks for playing!"
            
--
-- MAIN
--
main = do
    gameMenu