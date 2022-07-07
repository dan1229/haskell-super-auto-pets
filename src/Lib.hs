module Lib where
import System.Random (randomRIO)
import Data.String (IsString)

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

goldInitial :: Cost
goldInitial = (Cost 15)

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
    putStrBar
    pet1 <- getPet allPets
    pet2 <- getPet allPets
    pet3 <- getPet allPets
    let petChoices = [pet1, pet2, pet3]
    putStrLn $ "1. " ++ show (petChoices !! 0)
    putStrLn $ "2. " ++ show (petChoices !! 1)
    putStrLn $ "3. " ++ show (petChoices !! 2)

    -- deal with pet selection and gold
    putStrLn $ "\nYou have $" ++ show goldRemaining ++ " gold remaining."
    choice <- keepAskingWhere "Select your pet:" (betweenInclusive 1 3)
    let pet = petChoices !! (choice - 1) 
    putStrLn $ "\nYou've selected: " ++ show pet
    let roster' = insertPet pet (userRoster user)
    let user' = user{ userRoster=roster', userItemList=(userItemList user) }

    let goldRemaining' = goldRemaining - (petCost pet)
    if goldRemaining' > (Cost 0)
        then startPetSelection user' goldRemaining'
    else startBattle user'



startBattle :: User -> IO ()
startBattle user = do
    putStrBar
    putStrLn "YOUR TEAM"
    putStrLn $ show (userRoster user)

    -- Create enemy team
    -- TODO create their lineup using gold the same as the player
    petOpponent1 <- getPet allPets
    petOpponent2 <- getPet allPets
    petOpponent3 <- getPet allPets
    let petsOpponent = [petOpponent1, petOpponent2, petOpponent3]
    putStrLn "ENEMY TEAM"
    putStrLn $ show (petsOpponent)

    -- TODO BATTLE


putStrBar :: IO ()
putStrBar = do
    putStrLn "\n==================="

-- keep asking
keepAsking :: Read a => String -> IO a
keepAsking s = do
  putStr s
  hFlush stdout
  mresult <- readMaybe <$> getLine
  case mresult of
    Nothing -> keepAsking s
    Just a -> pure a

keepAskingWhere :: Read a => String -> (a -> Bool) -> IO a
keepAskingWhere s p = do
  a <- keepAsking s
  if p a then pure a else keepAskingWhere s p

betweenInclusive :: Int -> Int -> Int -> Bool
betweenInclusive a b x = a <= x && x <= b

--
-- ATTRIBUTES
--
newtype Name = Name {getName::String} deriving (Show, IsString)
newtype Attack = Attack {getAttack::Int} deriving Show
newtype Health = Health {getHealth::Int} deriving Show
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

instance Show Cost where
    show (Cost x) = show x


--
-- PETS
--
data Pet = Pet
  { petName :: Name
  , petAttack :: Attack
  , petHealth :: Health
  , petCost :: Cost
  }
-- TODO how to do this? is this the best way to do this? is this necessary?
-- data AnimalType = Turtle | Ant | Penguin | etc...


instance Show Pet where
  show (Pet name attack health cost) = (getName name) ++ " $" ++ show (getCost cost) ++ " (A: " ++ show (getAttack attack) ++ ", H: " ++ show (getHealth health) ++ ")"


-- global list of pets
allPets :: [Pet]
allPets =
  [ Pet "Ralfy" (Attack 10) (Health 30) (Cost 5)
  , Pet "Teddy" (Attack 10)(Health 35) (Cost 5)
  , Pet "Fredd" (Attack 7) (Health 20) (Cost 5)
  , Pet "Neddd" (Attack 30) (Health 5) (Cost 5)
  , Pet "Edddy" (Attack 10) (Health 100) (Cost 5)
  , Pet "Kevly" (Attack 20) (Health 25) (Cost 5)
  , Pet "Renly" (Attack 10) (Health 15) (Cost 5)
  , Pet "Fedly" (Attack 3) (Health 30) (Cost 5)
  , Pet "Pengy" (Attack 90) (Health 90) (Cost 5)
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


--
-- USER
--
data User = User
  { userName :: String
  , userRoster :: Roster
  , userItemList :: ItemList
  }

data Roster = Roster
  { rosterPet1 :: Maybe Pet
  , rosterPet2 :: Maybe Pet
  , rosterPet3 :: Maybe Pet
  , rosterPet4 :: Maybe Pet
  , rosterPet5 :: Maybe Pet
  , rosterPet6 :: Maybe Pet
  } deriving Show


rosterEmpty :: Roster
rosterEmpty = Roster
  { rosterPet1 = Nothing
  , rosterPet2 = Nothing
  , rosterPet3 = Nothing
  , rosterPet4 = Nothing
  , rosterPet5 = Nothing
  , rosterPet6 = Nothing
  }


data Item = Item
  { itemName :: Name
  -- TODO how to model item effects?
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

