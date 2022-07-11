module Lib where
import System.Random (randomRIO)
import Data.String (IsString)

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isNothing)


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
    putStrBar
    pet1 <- getPet allPets
    pet2 <- getPet allPets
    pet3 <- getPet allPets
    let petChoices = [pet1, pet2, pet3]
    putStrLn $ "1. " ++ display (petChoices !! 0)
    putStrLn $ "2. " ++ display (petChoices !! 1)
    putStrLn $ "3. " ++ display (petChoices !! 2)

    -- deal with pet selection and gold
    putStrLn $ "\nYou have $" ++ show goldRemaining ++ " gold remaining."
    choice <- keepAskingWhere "Select your pet: " (betweenInclusive 1 3)
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
    -- TODO add team name?

    -- Create enemy team
    -- TODO create their lineup using gold the same as the player
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
        , rosterPet6 = Nothing
        }
    let
      userOpponent = User
        { userName = "OPPONENT"
        , userRoster = opponentRoster
        , userItemList = itemListEmpty
        }
    putStrLn "\nOPPONENT TEAM"
    putStrLn $ show (opponentRoster)

    -- TODO BATTLE
    battleRoster user userOpponent


battleRoster :: User -> User -> IO ()
battleRoster user1 user2 = do
  let r1pet = getRosterFirst (userRoster user1)
  let r2pet = getRosterFirst (userRoster user2)

  if isNothing r1pet
    then putStrLn "user1 LOSES"
    else putStrLn ""   -- wont compile without else?
  if isNothing r2pet
    then putStrLn "user2 LOSES"
    else putStrLn ""


  let user1Pet = fromJust r1pet
  let user2Pet = fromJust r2pet
  putStrLn $ userName user1 ++ ": " ++ display user1Pet
  putStrLn $ "vs."
  putStrLn $ userName user2 ++ ": " ++ display user2Pet

  -- battle pets
  let (user1Pet', user2Pet') = battlePets user1Pet user2Pet

  -- create new rosters (and users) and battle again
  -- roster{ rosterPet1=(Just p) }
  -- let user = User { userName=username, userRoster=rosterEmpty, userItemList=itemListEmpty }
  -- battleRoster user1' user2'
  putStrLn "BATTLE"


battlePets :: Pet -> Pet -> (Pet, Pet) -- dont hate me
battlePets p1 p2 = do
  putStrLn "BATTLE"

  -- ATTACK
  let p1HealthRemaining = getHealth (petHealthRemaining p1) - getAttack (petAttack p2)
  let p2HealthRemaining = getHealth (petHealthRemaining p2) - getAttack (petAttack p1)

  -- create new pets with new health
  let p1' = p1 { petName=(petName p1), petAttack=(petAttack p1), petHealth=(petHealth p1), petHealthRemaining=(Health p1HealthRemaining), petCost=(petCost p1)}
  let p2' = p2 { petName=(petName p2), petAttack=(petAttack p2), petHealth=(petHealth p2), petHealthRemaining=(Health p2HealthRemaining), petCost=(petCost p2)}

  -- check results
  if ((getHealth (petHealthRemaining p1) < 0) || (getHealth (petHealthRemaining p2) < 0))
    then (p1', p2')  -- one or both pets are dead
    else battlePets p1' p2'  -- not dead battle again




putStrBar :: IO ()
putStrBar = do
    putStrLn "\n==================="

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
newtype Name = Name {getName::String} deriving (Show, IsString)
newtype Attack = Attack {getAttack::Int} deriving Show
newtype Health = Health {getHealth::Int} deriving Show
newtype Cost = Cost {getCost::Int} deriving (Num, Ord, Eq)

class Display a where
  display :: a -> String

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
  , petHealthRemaining :: Health
  , petCost :: Cost
  }

mkPet :: Name -> Attack -> Health -> Cost -> Pet
mkPet name attack health cost = Pet name attack health health cost


instance Show Pet where  -- TODO replace all shows with displays
  show (Pet name attack health healthRemaining cost) = getName name ++ " $" ++ show (getCost cost) ++ " (A: " ++ show (getAttack attack) ++ ", H: " ++ "/" ++ show (getHealth health) ++ ")"

instance Display Pet where
  display (Pet name attack health healthRemaining cost) = getName name ++ " $" ++ show (getCost cost) ++ " (A: " ++ show (getAttack attack) ++ ", H: " ++ "/" ++ show (getHealth health) ++ ")"


allPets :: [Pet]
allPets =
  [ mkPet "Ralfy" (Attack 10) (Health 30) (Cost 5)
  , mkPet "Teddy" (Attack 10)(Health 35) (Cost 5)
  , mkPet "Fredd" (Attack 7) (Health 20) (Cost 5)
  , mkPet "Neddd" (Attack 30) (Health 5) (Cost 5)
  , mkPet "Edddy" (Attack 10) (Health 100) (Cost 5)
  , mkPet "Kevly" (Attack 20) (Health 25) (Cost 5)
  , mkPet "Renly" (Attack 10) (Health 15) (Cost 5)
  , mkPet "Fedly" (Attack 3) (Health 30) (Cost 5)
  , mkPet "Pengy" (Attack 90) (Health 90) (Cost 5)
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



getRosterFirst :: Roster -> Maybe Pet
getRosterFirst roster = case roster of
  Roster p _ _ _ _ _ -> p
  Roster Nothing p _ _ _ _ -> p
  Roster Nothing Nothing p _ _ _ -> p
  Roster Nothing Nothing Nothing p _ _ -> p
  Roster Nothing Nothing Nothing Nothing p _ -> p
  Roster Nothing Nothing Nothing Nothing Nothing p -> p
  Roster Nothing Nothing Nothing Nothing Nothing Nothing -> Nothing -- says this line is redundant?



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

-- TODO fix this
-- instance Display Roster where
--   display (Roster p1 p2 p3 p4 p5 p6) = (display Just p1) ++ (display Just p2) ++ (display Just p3) ++ (display Just p4)


--
-- ITEM
--
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

