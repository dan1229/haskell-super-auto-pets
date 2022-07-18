module Roster where


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


rosterToList :: Roster -> [Pet]
rosterToList Roster {..} = catMaybes [rosterPet1, rosterPet2, rosterPet3, rosterPet4, rosterPet5]


listToRoster :: [Pet] -> Roster
listToRoster pets = go 1 pets rosterEmpty
  where
    go _ [] r = r
    go n (x:xs) r = go (n + 1) xs (insertPetAt n x r)

  
instance Display Roster where
  display roster = "ROSTER: " ++ intercalate ", " (map display (rosterToList roster))


