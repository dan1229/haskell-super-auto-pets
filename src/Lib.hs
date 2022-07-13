{-# LANGUAGE RecordWildCards #-}
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
  printPetBattle user1Pet user2Pet

  -- battle pets
  let (user1Pet', user2Pet') = battlePets user1Pet user2Pet
  putStrLn "\nResult:"
  printPetBattle user1Pet' user2Pet'

  -- create updated rosters  - include 'dead' pets in roster to retain full original roster
  let r1' = replacePet user1Pet user1Pet' (userRoster user1)
  let r2' = replacePet user2Pet user2Pet' (userRoster user2)
  
  -- create updated users
  let user1' = user1 { userRoster = r1' }
  let user2' = user2 { userRoster = r2' }
  putStrLn $ display (userRoster user1')
  putStrLn $ display (userRoster user2')

  -- TODO detect if either player is out of pets
  -- battleRoster user1' user2'



battlePets :: Pet -> Pet -> (Pet, Pet) -- dont hate me
battlePets p1 p2 = do
  -- ATTACK
  let p1HealthRemaining = getHealth (petHealthRemaining p1) - getAttack (petAttack p2)
  let p2HealthRemaining = getHealth (petHealthRemaining p2) - getAttack (petAttack p1)

  -- create new pets with new health
  let p1' = p1 { petHealthRemaining = (Health p1HealthRemaining) }
  let p2' = p2 { petHealthRemaining = (Health p2HealthRemaining) }
  
  -- check results
  if ((getHealth (petHealthRemaining p1') < 0) || (getHealth (petHealthRemaining p2') < 0))
    then (p1', p2')  -- one or both pets are dead
    else battlePets p1' p2'  -- not dead battle again

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
newtype Id = Id {getId::Int} deriving (Num, Ord, Eq)
newtype Name = Name {getName::String} deriving (IsString, Eq)
newtype Emoji = Emoji {getEmoji::String} deriving (IsString, Eq)
newtype Attack = Attack {getAttack::Int} deriving (Num, Ord, Eq)
newtype Health = Health {getHealth::Int} deriving (Num, Ord, Eq)
newtype Cost = Cost {getCost::Int} deriving (Num, Ord, Eq)

class Attribute a where
  valid :: a -> Bool

instance Attribute Id where
  valid (Id x) = x > 0

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
  { petId :: Id
  , petName :: Name
  , petEmoji :: Emoji
  , petAttack :: Attack
  , petHealth :: Health
  , petHealthRemaining :: Health
  , petCost :: Cost
  }


instance Eq Pet where
   (Pet id name emoji attack healthRemaining health cost) == (Pet id' name' emoji' attack' healthRemaining' health' cost') = id == id'


mkPet :: Name -> Id -> Emoji -> Attack -> Health -> Cost -> Pet
mkPet name id emoji attack health cost = Pet
  { petId = id
  , petName = name
  , petEmoji = emoji
  , petAttack = attack
  , petHealth = health
  , petHealthRemaining = health
  , petCost = cost
  }


allPets :: [Pet]
allPets =
  [ mkPet "Ralfy" (Id 1) (Emoji "🐷") (Attack 10) (Health 30) (Cost 5)
  , mkPet "Teddy" (Id 2) (Emoji "🐻") (Attack 10)(Health 35) (Cost 5)
  , mkPet "Fredd" (Id 3) (Emoji "🐔") (Attack 7) (Health 20) (Cost 5)
  , mkPet "Neddd" (Id 4) (Emoji "🦇") (Attack 30) (Health 5) (Cost 5)
  , mkPet "Edddy" (Id 5) (Emoji "🦆") (Attack 10) (Health 100) (Cost 5)
  , mkPet "Kevly" (Id 6) (Emoji "🦅") (Attack 20) (Health 25) (Cost 5)
  , mkPet "Renly" (Id 7) (Emoji "🦁") (Attack 10) (Health 15) (Cost 5)
  , mkPet "Fedly" (Id 8) (Emoji "🦟") (Attack 3) (Health 30) (Cost 5)
  , mkPet "Slosom" (Id 9) (Emoji "🦥") (Attack 900) (Health 95) (Cost 12)
  , mkPet "SandyMan" (Id 10) (Emoji "🐌") (Attack 10) (Health 100) (Cost 5)
  , mkPet "Seth" (Id 11) (Emoji "🦄") (Attack 60) (Health 60) (Cost 6)
  , mkPet "Soupsir" (Id 12) (Emoji "🐉") (Attack 150) (Health 200) (Cost 9)
  , mkPet "Pengy" (Id 13) (Emoji "🐧") (Attack 90) (Health 90) (Cost 5)
  , mkPet "Pengy's evil brother" (Id 14) (Emoji "👹") (Attack 90) (Health 90) (Cost 5)
  ]


getPet :: [a] -> IO a
getPet xs = do
  idx <- randomRIO (0, length xs - 1)
  pure $ xs !! idx


-- Helper functions
insertPet :: Pet -> Roster -> Roster
insertPet p roster = case roster of
  Roster Nothing _ _ _ _ -> roster{ rosterPet1=(Just p) }
  Roster _ Nothing _ _ _ -> roster{ rosterPet2=(Just p) }
  Roster _ _ Nothing _ _ -> roster{ rosterPet3=(Just p) }
  Roster _ _ _ Nothing _ -> roster{ rosterPet4=(Just p) }
  Roster _ _ _ _ Nothing -> roster{ rosterPet5=(Just p) }
  _ -> error "OH NO"


getRosterFirst :: Roster -> Maybe Pet
getRosterFirst roster = getRosterFirstWhere roster (const True)


getRosterFirstWhere :: Roster -> (Pet -> Bool) -> Maybe Pet
getRosterFirstWhere roster cond = case roster of
  Roster (Just p) _ _ _ _ | cond p -> Just p
  Roster _ (Just p) _ _ _ | cond p  -> Just p
  Roster _ _ (Just p) _ _ | cond p  -> Just p
  Roster _ _ _ (Just p) _ | cond p  -> Just p
  Roster _ _ _ _ (Just p) | cond p  -> Just p
  Roster _ _ _ _ _ -> Nothing


healthPositive :: Pet -> Bool
healthPositive p = (petHealthRemaining p) > (Health 0)


replacePet :: Pet -> Pet -> Roster -> Roster
replacePet petToRemove petToAdd Roster {..} = Roster
  { rosterPet1 = doReplace rosterPet1
  , rosterPet2 = doReplace rosterPet2
  , rosterPet3 = doReplace rosterPet3
  , rosterPet4 = doReplace rosterPet4
  , rosterPet5 = doReplace rosterPet5
  }
  where
    doReplace mPet = if Just petToRemove == mPet then Just petToAdd else mPet



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
  }


rosterEmpty :: Roster
rosterEmpty = Roster
  { rosterPet1 = Nothing
  , rosterPet2 = Nothing
  , rosterPet3 = Nothing
  , rosterPet4 = Nothing
  , rosterPet5 = Nothing
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
-- how to make these look cleaner? i.e., multiline
instance Display Pet where
  display (Pet id name emoji attack health healthRemaining cost) = getName name ++ " " ++ getEmoji emoji ++ " $" ++ display cost ++ " (A: " ++ display attack ++ ", H: " ++ display healthRemaining ++ "/" ++ display health ++ ")"

instance Display Roster where -- how to handle 'nothing' here?
  display (Roster rp1 rp2 rp3 rp4 rp5) = "ROSTER: " ++ (display (fromJust rp1)) ++ ", " ++ (display (fromJust rp2)) ++ ", " ++ (display (fromJust rp3)) ++ ", "


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
  putStrLn $ display p1
  putStrLn $ "vs."
  putStrLn $ display p2
