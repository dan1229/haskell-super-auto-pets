module Lib where
import System.Random (randomRIO)
import Data.String (IsString)

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Control.Monad (when)

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
    printBar
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
        , rosterPet6 = Nothing
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
  let r1pet = getRosterFirst (userRoster user1)
  let r2pet = getRosterFirst (userRoster user2)

  when (isNothing r1pet) (putStrLn "user1 LOSES")
  when (isNothing r2pet) (putStrLn "user2 LOSES")

  let user1Pet = fromJust r1pet
  let user2Pet = fromJust r2pet
  putStrLn "\n"
  putStrLn $ userName user1 ++ ": " ++ display user1Pet
  putStrLn $ "vs."
  putStrLn $ userName user2 ++ ": " ++ display user2Pet

  -- battle pets
  let (user1Pet', user2Pet') = battlePets user1Pet user2Pet

  putStrLn "\nResult:"
  displayPets user1Pet' user2Pet'

  -- create updated rosters  - include 'dead' pets in roster to retain full original roster
  let r1' = replacePet user1Pet user1Pet' (userRoster user1)
  let r2' = replacePet user2Pet user2Pet' (userRoster user2)
  
  -- create updated users
  let user1' = user1 { userRoster = r1' }
  let user2' = user2 { userRoster = r2' }

  -- TODO detect if either player is out of pets
  battleRoster user1' user2'



battlePets :: Pet -> Pet -> (Pet, Pet) -- dont hate me
battlePets p1 p2 = do
  -- ATTACK
  let p1HealthRemaining = getHealth (petHealthRemaining p1) - getAttack (petAttack p2)
  let p2HealthRemaining = getHealth (petHealthRemaining p2) - getAttack (petAttack p1)

  -- create new pets with new health
  let p1' = Pet { petName=(petName p1), petAttack=(petAttack p1), petHealth=(petHealth p1), petHealthRemaining=(Health p1HealthRemaining), petCost=(petCost p1)}
  let p2' = Pet { petName=(petName p2), petAttack=(petAttack p2), petHealth=(petHealth p2), petHealthRemaining=(Health p2HealthRemaining), petCost=(petCost p2)}

  -- check results
  if ((getHealth (petHealthRemaining p1') < 0) || (getHealth (petHealthRemaining p2') < 0))
    then (p1', p2')  -- one or both pets are dead
    else battlePets p1' p2'  -- not dead battle again

-- print/display
displayPets :: Pet -> Pet -> IO ()
displayPets p1 p2 = do
  putStrLn $ display p1
  putStrLn $ "vs."
  putStrLn $ display p2

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


--
-- ATTRIBUTES
--
newtype Name = Name {getName::String} deriving (IsString)
newtype Attack = Attack {getAttack::Int} 
newtype Health = Health {getHealth::Int} deriving (Num, Ord, Eq)
newtype Cost = Cost {getCost::Int} deriving (Num, Ord, Eq)

class Attribute a where
  valid :: a -> Bool

instance Attribute Name where
  valid (Name x) = x /= ""

instance Attribute Attack where
  valid (Attack x) = x > 0

instance Attribute Health where
  valid (Health x) = x > 0

instance Attribute Cost where
  valid (Cost x) = x >= 0

--
-- PETS
--
data Pet = Pet
  { petName :: Name
  , petAttack :: Attack
  , petHealth :: Health
  , petHealthRemaining :: Health
  , petCost :: Cost
  }

mkPet :: Name -> Attack -> Health -> Cost -> Pet
mkPet name attack health cost = Pet
  { petName = name
  , petAttack = attack
  , petHealth = health
  , petHealthRemaining = health
  , petCost = cost
  }



allPets :: [Pet]
allPets =
  [ mkPet "Ralfy" (Attack 10) (Health 30) (Cost 5)
  , mkPet "Teddy" (Attack 10)(Health 35) (Cost 5)
  , mkPet "Fredd" (Attack 7) (Health 20) (Cost 5)
  , mkPet "Neddd" (Attack 30) (Health 5) (Cost 5)
  , mkPet "Edddy" (Attack 10) (Health 100) (Cost 5)
  , mkPet "Kevly" (Attack 20) (Health 25) (Cost 5)
  , mkPet "Renly" (Attack 10) (Health 15) (Cost 5)
  , mkPet "Fedly" (Attack 3) (Health 30) (Cost 5)
  , mkPet "Slosom" (Attack 900) (Health 95) (Cost 12)
  , mkPet "SandyMan" (Attack 10) (Health 100) (Cost 5)
  , mkPet "Seth" (Attack 60) (Health 60) (Cost 6)
  , mkPet "Pengy" (Attack 90) (Health 90) (Cost 5)
  , mkPet "Soupsir" (Attack 150) (Health 200) (Cost 9)
  , mkPet "Pengy's evil brother" (Attack 90) (Health 90) (Cost 5)
  ]


getPet :: [a] -> IO a
getPet xs = do
  idx <- randomRIO (0, length xs - 1)
  pure $ xs !! idx


insertPet :: Pet -> Roster -> Roster
insertPet p roster = case roster of
  Roster Nothing _ _ _ _ _ -> roster{ rosterPet1=(Just p) }
  Roster _ Nothing _ _ _ _ -> roster{ rosterPet2=(Just p) }
  Roster _ _ Nothing _ _ _ -> roster{ rosterPet3=(Just p) }
  Roster _ _ _ Nothing _ _ -> roster{ rosterPet4=(Just p) }
  Roster _ _ _ _ Nothing _ -> roster{ rosterPet5=(Just p) }
  Roster _ _ _ _ _ Nothing -> roster{ rosterPet6=(Just p) }
  _ -> error "OH NO"


-- TODO update to account for healthRemaining
-- such that it will get the first pet with healthRemaining > 0
-- getRosterFirst :: Roster -> Maybe Pet
-- getRosterFirst roster = case roster of
--   Roster (Just p) _ _ _ _ _ -> Just p
--   Roster Nothing (Just p) _ _ _ _ -> Just p
--   Roster Nothing Nothing (Just p) _ _ _ -> Just p
--   Roster Nothing Nothing Nothing (Just p) _ _ -> Just p
--   Roster Nothing Nothing Nothing Nothing (Just p) _ -> Just p
--   Roster Nothing Nothing Nothing Nothing Nothing (Just p) -> Just p
--   Roster Nothing Nothing Nothing Nothing Nothing Nothing -> Nothing




getRosterFirst :: Roster -> Maybe Pet
getRosterFirst roster = getRosterFirstWhere roster (const True)


getRosterFirstWhere :: Roster -> (Health -> Bool) -> Maybe Pet
getRosterFirstWhere roster cond = case roster of
  Roster (Just p) _ _ _ _ _ -> if cond (petHealthRemaining p) then Just p else Nothing
  Roster Nothing (Just p) _ _ _ _ -> Just p
  Roster Nothing Nothing (Just p) _ _ _ -> Just p
  Roster Nothing Nothing Nothing (Just p) _ _ -> Just p
  Roster Nothing Nothing Nothing Nothing (Just p) _ -> Just p
  Roster Nothing Nothing Nothing Nothing Nothing (Just p) -> Just p
  Roster Nothing Nothing Nothing Nothing Nothing Nothing -> Nothing


healthPositive :: Health -> Bool
healthPositive health = health > (Health 0)


----


replacePet :: Pet -> Pet -> Roster -> Roster
replacePet petToRemove petToAdd roster = do
  -- TODO
  roster


--
-- USER
--
data User = User
  { userName :: String
  , userRoster :: Roster
  , userItemList :: ItemList
  }

--
-- ROSTER
--
data Roster = Roster
  { rosterPet1 :: Maybe Pet
  , rosterPet2 :: Maybe Pet
  , rosterPet3 :: Maybe Pet
  , rosterPet4 :: Maybe Pet
  , rosterPet5 :: Maybe Pet
  , rosterPet6 :: Maybe Pet
  }


rosterEmpty :: Roster
rosterEmpty = Roster
  { rosterPet1 = Nothing
  , rosterPet2 = Nothing
  , rosterPet3 = Nothing
  , rosterPet4 = Nothing
  , rosterPet5 = Nothing
  , rosterPet6 = Nothing
  }


--
-- ITEM
--
data Item = Item
  { itemName :: Name
  }

data ItemList = ItemList
  { itemListItem1 :: Maybe Item
  , itemListItem2 :: Maybe Item
  , itemListItem3 :: Maybe Item
  }

itemListEmpty :: ItemList
itemListEmpty = ItemList
  { itemListItem1 = Nothing
  , itemListItem2 = Nothing
  , itemListItem3 = Nothing
  }


--
-- DISPLAY
--

class Display a where
  display :: a -> String

instance Display Attack where
  display (Attack x) = show x

instance Display Health where
  display (Health x) = show x

instance Display Cost where
  display (Cost x) = show x

-- TODO update to use emoji
instance Display Pet where
  display (Pet name attack health healthRemaining cost) = getName name ++ " $" ++ display cost ++ " (A: " ++ display attack ++ ", H: " ++ display healthRemaining ++ "/" ++ display health ++ ")"

instance Display Roster where
  display (Roster rp1 rp2 rp3 rp4 rp5 rp6) = "ROSTER: " ++ (display (fromJust rp1)) ++ ", " 



-- PRINT
printBar :: IO ()
printBar = do
    putStrLn "\n==================="

printPetList :: [Pet] -> IO ()
printPetList pets = do
    mapM_ (\(idx, choice) -> putStrLn $ show idx ++ ". " ++ display choice) (zip [1..] pets) 
