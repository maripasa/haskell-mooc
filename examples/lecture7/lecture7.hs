import Data.List
import Data.Ord


-- "Design Patterns for Typed Functional Programming"
--
-- 1. Instead of comments, you could use boxed types

data Plate = Plate String
  deriving (Show, Eq)

-- registerCar :: String -> Plate -> CarRegistry -> CarRegistry
-- Now it's pretty
-- And
-- You can make smart constructors and restrict the operations you can do in Plate
-- A bit like oop's encapsulation

-- *Tangent:
-- lsp shows "why newtype"
-- https://stackoverflow.com/questions/2649305/why-is-there-data-and-newtype-in-haskell
-- newtypes are faster AND strict
-- data are slower but lazy
-- let's follow the mooc
  
data Person = Person {name :: String, age :: Int}
  deriving Show

data SortOrder = Ascending | Descending
data SortField = Name | Age

sortByField :: SortField -> [Person] -> [Person]
sortByField Name ps = sortBy (comparing name) ps
sortByField Age ps = sortBy (comparing age) ps

sortPersons :: SortField -> SortOrder -> [Person] -> [Person]
sortPersons field Ascending ps = sortByField field ps
sortPersons field Descending ps = reverse (sortByField field ps)

persons = [Person "Fridolf" 73, Person "Greta" 60, Person "Hans" 65]

-- More descriptive arguments, like enums.

-- Monoids. What is a monoid?
-- Associativity -> Order isn't important (f x (f y z)) == (f (f x y) z)
-- Semigroups -> a associative (function | operation) forms a semigroup. Each type has one of these, types like int have boxes such as Min, Product and Sum
-- Monoids -> A semigroup with an neutral element : A zero, an id. 0, 1, [] (+, *, ++)
-- Monoid represents monoids.

{-
instance Num a => Monoid (Sum a) where
  mempty = Sum 0

instance Num a => Monoid (Product a) where
  mempty = Product 1

instance Monoid [] where
  mempty = []
-}

-- ... why tho?
-- 7.3.4 Why?
-- Because yes! And it helps with reduce... That's what I got, you don't need'em, but you'll bump'em

