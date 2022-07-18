module Pet where
import Attributes
import Roster
import Print

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
   p1 == p2 = petId p1 == petId p2


mkPet :: Name -> Id -> Emoji -> Attack -> Health -> Cost -> Pet
mkPet name pid emoji attack health cost = Pet
  { petId = pid
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



instance Display Pet where
  display (Pet id name emoji attack health healthRemaining cost) = getName name ++ " " ++ getEmoji emoji ++ " $" ++ display cost ++ " (A: " ++ display attack ++ ", H: " ++ display healthRemaining ++ "/" ++ display health ++ ")"


getPet :: [a] -> IO a
getPet xs = do
  idx <- randomRIO (0, length xs - 1)
  pure $ xs !! idx


-- Helper functions
insertPet :: Pet -> Roster -> Roster
insertPet p roster = case roster of
  Roster Nothing _ _ _ _ -> roster{ rosterPet1 = Just p }
  Roster _ Nothing _ _ _ -> roster{ rosterPet2 = Just p }
  Roster _ _ Nothing _ _ -> roster{ rosterPet3 = Just p }
  Roster _ _ _ Nothing _ -> roster{ rosterPet4 = Just p }
  Roster _ _ _ _ Nothing -> roster{ rosterPet5 = Just p }
  _ -> error "OH NO"

modifyPetAt :: Int -> (Maybe Pet -> Maybe Pet) -> Roster -> Roster
modifyPetAt n f r@Roster{..} = case n of
  1 -> r { rosterPet1 = f rosterPet1 }
  2 -> r { rosterPet2 = f rosterPet2 }
  3 -> r { rosterPet3 = f rosterPet3 }
  4 -> r { rosterPet4 = f rosterPet4 }
  5 -> r { rosterPet5 = f rosterPet5 }
  _ -> error $ "Unknown roster index, tried to modify pet at " <> show n

insertPetAt :: Int -> Pet -> Roster -> Roster
insertPetAt n p = modifyPetAt n (const (Just p))


isRosterEmpty :: Roster -> Bool
isRosterEmpty roster = isNothing (getRosterFirstWhere roster healthPositive)


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


