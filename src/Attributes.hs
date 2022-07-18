module Attributes where



--
-- ATTRIBUTES
--
newtype Id = Id { getId::Int }
  deriving (Num, Ord, Eq)

newtype Name = Name { getName::String }
  deriving (IsString, Eq)

newtype Emoji = Emoji { getEmoji::String }
  deriving (IsString, Eq)

newtype Attack = Attack { getAttack::Int }
  deriving (Num, Ord, Eq)

newtype Health = Health { getHealth::Int }
  deriving (Num, Ord, Eq)

newtype Cost = Cost { getCost::Int }
  deriving (Num, Ord, Eq)


class Attribute a where
  valid :: a -> Bool

--
-- VALID
--

instance Attribute Id where
  valid (Id x) = x > 0

instance Attribute Name where
  valid (Name x) = x /= ""

instance Attribute Attack where
  valid (Attack x) = x > 0

instance Attribute Health where
  valid (Health x) = x > 0

instance Attribute Cost where
  valid (Cost x) = x >= 0



--
-- DISPLAY
--
class Display a where
  display :: a -> String

instance Display Attack where
  display (Attack x) = show x

instance Display Health where
  display (Health x) = show x

instance Display Cost where
  display (Cost x) = show x
