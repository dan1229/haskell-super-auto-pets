module Print where


--
-- PRINT
--
printBar :: IO ()
printBar = do
    putStrLn "\n==================="


printPetList :: [Pet] -> IO ()
printPetList pets = do
    mapM_ (\(idx, choice) -> putStrLn $ show idx ++ ". " ++ display choice) (zip [1..] pets) 


printPetBattle :: Pet -> Pet -> IO ()
printPetBattle p1 p2 = do
  putStrLn $ display p1 ++ "vs. " ++ display p2
