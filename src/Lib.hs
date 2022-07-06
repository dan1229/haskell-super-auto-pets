module Lib where
import System.Random
import Data.String (IsString)


--
-- ROUNDS
--
startRound :: Int -> IO ()
startRound round = do
    putStrLn $ "Round " ++ show round ++ " starting..."
    pet <- getPet allPets
    print pet
    -- TODO select three pets to present randomly
    -- indPet1 <- randomIO numPets
    -- let indPet2 = rng numPets
    -- let indPet3 = rng numPets
    -- let petChoices = [getPet indPet1, getPet indPet2, getPet indPet3]
    -- putStrLn $ show indPet1 ++ show indPet2 ++ show indPet3
    -- putStrLn $ "Pets\n" ++ show indPet1
    -- TODO choose pets, deal with gold, etc.



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
-- data AnimalType = Turtle | Ant | Penguin | etc... -- TODO how to do this? is this the best way to do this? is this necessary?
data Pet = Pet Name Attack Defense Health Cost deriving Show

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

-- TODO add some kind of semigroup/monoid for combining animals when you drag them ontop of one another
-- GOOSE



-- Types for 'user' states
    -- best way to have these as a global state?
-- overall health
-- money - dont need, just get 10 every round
-- round - current round number only maybe

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