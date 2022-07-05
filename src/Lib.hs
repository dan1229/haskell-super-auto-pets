module Lib where


-- Types for pet
newType Name = String
newtype Attack = Int
newtype Defense = Int  -- how does defense work? should we subtract it from attack? should this be a more complex type?
newtype Health = Int
newtype Cost = Int

data Pet = Pet Name Attack Defense Health Cost deriving Show

mkPet :: Name -> Attack -> Defense -> Health -> Cost -> Maybe Pet
mkPet name attack defense health cost
-- TODO add error messages for specific issues
    | name /= "" && attack > 0 && defense > 0 && health > 0 = Just $ Pet name attack defense health cost
    | otherwise = Nothing

-- TODO add some kind of semigroup/monoid for combining animals when you drag them ontop of one another




-- Types for 'user' states
    -- best way to have these as a global state?
-- overall health
-- money - dont need, just get 10 every round
-- round - current round number only maybe



-- MAIN
main = do
  putStrLn "MAIN"
  -- TODO create pet instances, how to store them globally?