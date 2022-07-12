module Main where

import Lib

gameMenu :: User -> Int -> IO ()
gameMenu user round = do
    printBar
    putStrLn "Want to play?"
    putStrLn "1. Yes"
    putStrLn "2. No (exit)"
    choice <- keepAskingWhere ">> " (betweenInclusive 1 2)

    -- TODO add team name?

    if choice == 1
        then do
            startRound user round
            gameMenu user (round + 1)
    else putStrLn "Thanks for playing!"
            
--
-- MAIN
--
main = do
    printBar
    putStrLn "What's your name?"
    username <- getLine
    let user = User { userName=username, userRoster=rosterEmpty, userItemList=itemListEmpty }

    putStrLn $ "\nWelcome " ++ (userName user) ++ "!"
    gameMenu user 1