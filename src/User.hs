
module User where


--
-- USER
--
data User = User
  { userName :: String
  , userRoster :: Roster
  , userItemList :: ItemList
  }
