module Lib where
import System.Random
import Data.String (IsString)
import Data.Maybe (fromJust)


--
-- ROUNDS
--
startRound :: User -> Int -> IO ()
startRound user round = do
    putStrLn $ "\nRound " ++ show round ++ " starting..."

    selectPets user
    -- TODO BATTLE


selectPets :: User -> IO ()
selectPets user = do
    let goldTotal = 10
    let goldRemaining = goldTotal

    pet1 <- getPet allPets
    pet2 <- getPet allPets
    pet3 <- getPet allPets
    putStrLn $ "1. " ++ show (fromJust pet1) -- is fromJust bad?
    putStrLn $ "2. " ++ show (fromJust pet2)
    putStrLn $ "3. " ++ show (fromJust pet3)

    -- TODO deal with pet selection, gold, etc.
    putStrLn $ "\nYou have " ++ show goldRemaining ++ " gold remaining."


--
-- ATTRIBUTES
--
newtype Name = Name {getName::String} deriving (Show, IsString)
newtype Attack = Attack {getAttack::Int} deriving Show
newtype Defense = Defense {getDefense::Int} deriving Show  -- how does defense work? should we subtract it from attack? should this be a more complex type?
newtype Health = Health {getHealth::Int} deriving Show
newtype Cost = Cost {getCost::Int} deriving Show

class Attribute a where
    valid :: a -> Bool

instance Attribute Name where
    valid (Name x) = x /= ""

instance Attribute Attack where
    valid (Attack x) = x > 0

instance Attribute Defense where
    valid (Defense x) = x > 0

instance Attribute Health where
    valid (Health x) = x > 0

instance Attribute Cost where
    valid (Cost x) = x >= 0


--
-- PETS
--
data Pet = Pet Name Attack Defense Health Cost
-- data AnimalType = Turtle | Ant | Penguin | etc... -- TODO how to do this? is this the best way to do this? is this necessary?

instance Show Pet where
  show (Pet name attack defense health cost) = (getName name) ++ " $" ++ show (getCost cost) ++ " (A: " ++ show (getAttack attack) ++ ", D: " ++ show (getDefense defense) ++ ", H: " ++ show (getHealth health) ++ ")"

mkPet :: Name -> Attack -> Defense -> Health -> Cost -> Maybe Pet
mkPet name attack defense health cost
    -- TODO add error messages for specific issues
    | valid name && valid attack && valid defense && valid health && valid cost = Just $ Pet name attack defense health cost
    | otherwise = Nothing


-- global list of pets
allPets :: [Maybe Pet]
allPets =
  [ mkPet "Ralph" (Attack 5) (Defense 10) (Health 20) (Cost 5)
  , mkPet "Teddy" (Attack 10) (Defense 5) (Health 20) (Cost 5)
  , mkPet "Fredd" (Attack 7) (Defense 8) (Health 20) (Cost 5)
  , mkPet "Neddd" (Attack 30) (Defense 30) (Health 5) (Cost 5)
  , mkPet "Edddy" (Attack 10) (Defense 10) (Health 100) (Cost 5)
  , mkPet "Kedly" (Attack 20) (Defense 5) (Health 25) (Cost 5)
  ]


getPet :: [a] -> IO a
getPet xs = do
  idx <- randomRIO (0, length xs - 1)
  pure $ xs !! idx


--
-- USER
--
data User = User
  { userName :: String
  , userRoster :: Roster
  }

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


-- Types for 'user' states
    -- best way to have these as a global state?
-- overall health
-- money - dont need, just get 10 every round
-- round - current round number only maybe
