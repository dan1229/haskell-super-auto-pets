{-# LANGUAGE RecordWildCards, DerivingStrategies #-}

module Lib where
import System.Random (randomRIO)
import Data.String (IsString)

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, fromJust, isNothing, catMaybes)
import Data.List (intercalate)
import Control.Monad (when)
import Control.Concurrent (threadDelay)

import Attributes
import Item
import Pet
import Print
import Roster
import User

goldInitial :: Cost
goldInitial = Cost 15

--
-- GAME FLOW
--
startRound :: User -> Int -> IO ()
startRound user round = do
    putStrLn $ "Round " ++ show round ++ " starting..."
    startPetSelection user goldInitial


startPetSelection :: User -> Cost -> IO ()
startPetSelection user goldRemaining = do
    -- list out 3 random choices
    putStrLn "\n"
    pet1 <- getPet allPets
    pet2 <- getPet allPets
    pet3 <- getPet allPets
    let petChoices = [pet1, pet2, pet3]
    printPetList petChoices

    -- deal with pet selection and gold
    putStrLn $ "\nYou have $" ++ display goldRemaining ++ " gold remaining."
    choice <- keepAskingWhere "Select your pet: " (betweenInclusive 1 3)
    let pet = petChoices !! (choice - 1) 
    putStrLn $ "\nYou've selected: " ++ display pet
    let roster' = insertPet pet (userRoster user)
    let user' = user{ userRoster=roster', userItemList=(userItemList user) }

    let goldRemaining' = goldRemaining - (petCost pet)
    if goldRemaining' > (Cost 0)
        then startPetSelection user' goldRemaining'
    else startBattle user'



startBattle :: User -> IO ()
startBattle user = do
    printBar
    putStrLn "YOUR TEAM"
    putStrLn $ display (userRoster user)
    
    -- Create opponent team
    petOpponent1 <- getPet allPets
    petOpponent2 <- getPet allPets
    petOpponent3 <- getPet allPets
    let 
      opponentRoster = Roster
        { rosterPet1 = Just petOpponent1
        , rosterPet2 = Just petOpponent2
        , rosterPet3 = Just petOpponent3
        , rosterPet4 = Nothing
        , rosterPet5 = Nothing
        }
    let
      userOpponent = User
        { userName = "OPPONENT"
        , userRoster = opponentRoster
        , userItemList = itemListEmpty
        }
    putStrLn "\nOPPONENT TEAM"
    putStrLn $ display (opponentRoster)

    -- BATTLE
    battleRoster user userOpponent


battleRoster :: User -> User -> IO ()
battleRoster user1 user2 = do
  -- get pets
  printBar
  putStrLn "NEW CHALLENGER:"
  let r1pet = getRosterFirstWhere (userRoster user1) healthPositive
  let r2pet = getRosterFirstWhere (userRoster user2) healthPositive
  let user1Pet = fromJust r1pet
  let user2Pet = fromJust r2pet
  printPetBattle user1Pet user2Pet

  -- battle pets
  let (user1Pet', user2Pet') = battlePets user1Pet user2Pet
  putStrLn "\nResult:"
  printPetBattle user1Pet' user2Pet'

  -- create updated rosters
  let r1' = replacePet user1Pet user1Pet' (userRoster user1)
  let r2' = replacePet user2Pet user2Pet' (userRoster user2)
  
  -- create updated users
  let user1' = user1 { userRoster = r1' }
  let user2' = user2 { userRoster = r2' }
  
  threadDelay 3000000 --sleep for a million microseconds, or three seconds

  -- detect if either player is out of pets
  let user1Res = isRosterEmpty (userRoster user1')
  let user2Res = isRosterEmpty (userRoster user2')
  printBar
  if user1Res == True && user2Res == True
    then putStrLn $ "\n**\nIT'S A TIE!!!\n**"
  else if user1Res == True
    then putStrLn $ "\n**\n" ++ (userName user1') ++ " LOSES\n**"
  else if user2Res == True
    then putStrLn $ "\n**\n" ++ (userName user2') ++ " LOSES\n**"
  else battleRoster user1' user2'

-- TODO add data type for (Pet, Pet)
-- remove recursion
battlePets :: Pet -> Pet -> (Pet, Pet) -- dont hate me
battlePets p1 p2 = do
  -- ATTACK
  let p1HealthRemaining = getHealth (petHealthRemaining p1) - getAttack (petAttack p2)
  let p2HealthRemaining = getHealth (petHealthRemaining p2) - getAttack (petAttack p1)

  -- create new pets with new health
  let p1' = p1 { petHealthRemaining = (Health p1HealthRemaining) }
  let p2' = p2 { petHealthRemaining = (Health p2HealthRemaining) }
  
  -- check results
  if getHealth (petHealthRemaining p1') < 0 || getHealth (petHealthRemaining p2') < 0
    then (p1', p2')  -- one or both pets are dead
    else battlePets p1' p2'  -- not dead battle again


-- TODO build something more recursive like this
-- utilitze 'Result' object
--
-- battlePets :: Pet -> Pet -> (Pet, Pet)
-- battlePets p1 p2 = (update p1 p2, update p2 p1)
--   where
--     update defender attacker = defender { pHealth = pHealth defender - pAttack attacker }

-- battleRoster :: Roster -> Roster -> Result
-- battleRoster r1 r2 = go (rosterToList r1) (rosterToList r2)
--   where
--     go [] [] = Tie
--     go [] _ = Lose
--     go _ [] = Win
--     go (x:xs) (y:ys) =
--       let (x', y') = battlePets x y
--        in go (update x' xs) (update y' ys)
--     update x xs = if pHealth x > 0 then x:xs else xs
    

-- keep asking
keepAskingWhere :: Read a => String -> (a -> Bool) -> IO a
keepAskingWhere s p = do
  putStr s
  hFlush stdout
  mresult <- readMaybe <$> getLine
  case mresult of
    Nothing -> keepAskingWhere s p
    Just a -> if p a then pure a else keepAskingWhere s p


keepAsking :: Read a => String -> IO a
keepAsking s = keepAskingWhere s (const True)


betweenInclusive :: Int -> Int -> Int -> Bool
betweenInclusive a b x = a <= x && x <= b



