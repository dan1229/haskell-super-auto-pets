module Item where

--
-- ITEM
--
data Item = Item
  { itemName :: Name
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

