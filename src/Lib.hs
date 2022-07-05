module Lib where

--
-- ROUNDS
--


startRound :: Int -> IO ()
startRound round =
    putStrLn $ "Starting round..." ++ show round

--
-- PETS
--

type Name = String
type Attack = Int
type Defense = Int  -- how does defense work? should we subtract it from attack? should this be a more complex type?
type Health = Int
type Cost = Int

data Pet = Pet Name Attack Defense Health Cost deriving Show

mkPet :: Name -> Attack -> Defense -> Health -> Cost -> Maybe Pet
mkPet name attack defense health cost
-- TODO add error messages for specific issues
    | name /= "" && attack > 0 && defense > 0 && health > 0 = Just $ Pet name attack defense health cost
    | otherwise = Nothing


-- global list of pets
getPet :: Int -> Maybe Pet
getPet 0 = mkPet "Ralph" 5 5 20 5
-- TODO more pets
f _ = Nothing


-- TODO add some kind of semigroup/monoid for combining animals when you drag them ontop of one another




-- Types for 'user' states
    -- best way to have these as a global state?
-- overall health
-- money - dont need, just get 10 every round
-- round - current round number only maybe



