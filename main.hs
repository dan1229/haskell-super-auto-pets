module Main where



-- Types for animal states
-- Attack
-- Defense (lowers incoming attack)
-- Health
-- Cost?
{- example of 'smart constructor' from book
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing
-}
newtype Attack = Int
newtype Defense = Int


-- Types for animals
-- dog, cat, ant, pig, alligator, duck beaver





-- Types for 'user' states - best way to have these as 'globals'?
-- overall health
-- money



-- MAIN



fibs :: [Integer]
fibs = fmap fib [1, 2 ..] 


fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)


main = do
  putStrLn "MAIN"