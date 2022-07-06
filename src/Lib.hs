module Lib where
import System.Random
import Data.String (IsString)
import Data.Maybe (fromJust)


--
-- GAME FLOW
--
startRound :: User -> Int -> IO ()
startRound user round = do
    putStrLn $ "\nRound " ++ show round ++ " starting..."
    startPetSelection user
    startBattle user


startPetSelection :: User -> IO ()
startPetSelection user = do
    let goldTotal = 10
    let goldRemaining = goldTotal

    -- list out 3 random choices
    pet1 <- getPet allPets
    pet2 <- getPet allPets
    pet3 <- getPet allPets
    let petChoices = [fromJust pet1, fromJust pet2, fromJust pet3]
    putStrLn $ "1. " ++ show (petChoices !! 0) -- is fromJust bad?
    putStrLn $ "2. " ++ show (petChoices !! 1)
    putStrLn $ "3. " ++ show (petChoices !! 2)

    -- TODO deal with pet selection, gold, etc.
    putStrLn $ "\nYou have " ++ show goldRemaining ++ " gold remaining."
    putStrLn $ "Select pet: "
    inputPet <- getLine 
    let choice = (read inputPet :: Int)
    putStrLn $ show $ petChoices !! (choice - 1) -- TODO check input


startBattle :: User -> IO ()
startBattle user = do
    putStrLn "\n==========="
    putStrLn $ show (userRoster user)
    putStrLn "\nBATTLE"
    putStrLn "BATTLE"
    putStrLn "BATTLE"
    -- TODO BATTLE


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
-- TODO how to do this? is this the best way to do this? is this necessary?
-- data AnimalType = Turtle | Ant | Penguin | etc...


instance Show Pet where
  show (Pet name attack defense health cost) = (getName name) ++ " $" ++ show (getCost cost) ++ " (A: " ++ show (getAttack attack) ++ ", D: " ++ show (getDefense defense) ++ ", H: " ++ show (getHealth health) ++ ")"

mkPet :: Name -> Attack -> Defense -> Health -> Cost -> Maybe Pet
mkPet name attack defense health cost
    | valid name && valid attack && valid defense && valid health && valid cost = Just $ Pet name attack defense health cost
    | otherwise = Nothing


-- global list of pets
allPets :: [Maybe Pet]
allPets =
  [ mkPet "Ralfy" (Attack 10) (Defense 20) (Health 30) (Cost 5)
  , mkPet "Teddy" (Attack 10) (Defense 5) (Health 35) (Cost 5)
  , mkPet "Fredd" (Attack 7) (Defense 8) (Health 20) (Cost 5)
  , mkPet "Neddd" (Attack 30) (Defense 30) (Health 5) (Cost 5)
  , mkPet "Edddy" (Attack 10) (Defense 10) (Health 100) (Cost 5)
  , mkPet "Kevly" (Attack 20) (Defense 20) (Health 25) (Cost 5)
  , mkPet "Renly" (Attack 10) (Defense 15) (Health 15) (Cost 5)
  , mkPet "Fedly" (Attack 3) (Defense 25) (Health 30) (Cost 5)
  , mkPet "Pengy" (Attack 90) (Defense 90) (Health 90) (Cost 5)
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
  -- TODO health?
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
