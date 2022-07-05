module Lib where

--
-- ROUNDS
--
startRound :: Int -> IO ()
startRound round = do
    putStrLn $ "Starting round... " ++ show round
    -- TODO choose pets, deal with gold, etc.



--
-- PETS
--
type Name = String
type Attack = Int
type Defense = Int  -- how does defense work? should we subtract it from attack? should this be a more complex type?
type Health = Int
type Cost = Int

-- data AnimalType = Turtle | Ant | Penguin | etc... -- TODO how to do this? is this the best way to do this? is this necessary?
data Pet = Pet Name Attack Defense Health Cost deriving Show

mkPet :: Name -> Attack -> Defense -> Health -> Cost -> Maybe Pet
mkPet name attack defense health cost
    -- TODO add error messages for specific issues
    | name /= "" && attack > 0 && defense > 0 && health > 0 = Just $ Pet name attack defense health cost
    | otherwise = Nothing


-- global list of pets
numPets = 6
getPet :: Int -> Maybe Pet
getPet 0 = mkPet "Ralph" 5 10 20 5  -- TODO how to require the types? i.e., "Attack 5"
getPet 1 = mkPet "Teddy" 10 5 20 5
getPet 2 = mkPet "Fredd" 7 8 20 5
getPet 3 = mkPet "Neddd" 30 30 5 5
getPet 4 = mkPet "Edddy" 10 10 100 5
getPet 5 = mkPet "Kedly" 20 5 25 5
-- TODO more pets
f _ = Nothing


-- TODO add some kind of semigroup/monoid for combining animals when you drag them ontop of one another
-- GOOSE



-- Types for 'user' states
    -- best way to have these as a global state?
-- overall health
-- money - dont need, just get 10 every round
-- round - current round number only maybe



