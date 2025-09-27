-- Exercise set 7

module Set7 where

import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Data.Semigroup
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: you'll find below the types Time, Distance and Velocity,
-- which represent time, distance and velocity in seconds, meters and
-- meters per second.
--
-- Implement the functions below.

data Distance = Distance Double
  deriving (Show, Eq)

data Time = Time Double
  deriving (Show, Eq)

data Velocity = Velocity Double
  deriving (Show, Eq)

-- velocity computes a velocity given a distance and a time
velocity :: Distance -> Time -> Velocity
velocity (Distance distance) (Time time) = Velocity (distance / time)

-- travel computes a distance given a velocity and a time
travel :: Velocity -> Time -> Distance
travel (Velocity velocity) (Time time) = Distance (velocity * time)

------------------------------------------------------------------------------
-- Ex 2: let's implement a simple Set datatype. A Set is a list of
-- unique elements. The set is always kept ordered.
--
-- Implement the functions below. You might need to add class
-- constraints to the functions' types.
-- Examples: member 'a' (Set ['a','b','c'])  ==>  True
--   add 2 (add 3 (add 1 emptySet))  ==>  Set [1,2,3]
--   add 1 (add 1 emptySet)  ==>  Set [1]

data Set a = Set [a]
  deriving (Show, Eq)

-- emptySet is a set with no elements
emptySet :: Set a
emptySet = Set []

-- member tests if an element is in a set
member :: (Eq a) => a -> Set a -> Bool
member a (Set xs) = elem a xs

-- add a member to a set
add :: (Ord a) => a -> Set a -> Set a
add a (Set []) = Set [a]
add a s@(Set xs)
  | elem a xs = s
  | otherwise = Set (sort (xs ++ [a]))

-- Actually, they did like I did earlier!

{-

-- add a member to a set
add :: Ord a => a -> Set a -> Set a
add a (Set xs) = Set (go a xs)
  where go a [] = [a]
        go a (x:xs)
          | a == x = x:xs
          | a > x = x : go a xs
          | a < x = a : x : xs

faster and stuff, also I think p@ reduces readability a bit
-}

------------------------------------------------------------------------------
-- Ex 3: a state machine for baking a cake. The type Event represents
-- things that can happen while baking a cake. The type State is meant
-- to represent the states a cake can be in.
--
-- Your job is to
--
--  * add new states to the State type
--  * and implement the step function
--
-- so that they have the following behaviour:
--
--  * Baking starts in the Start state
--  * A successful cake (reperesented by the Finished value) is baked
--    by first adding eggs, then adding flour and sugar (flour and
--    sugar can be added in which ever order), then mixing, and
--    finally baking.
--  * If the order of Events differs from this, the result is an Error cake.
--    No Events can save an Error cake.
--  * Once a cake is Finished, it stays Finished even if additional Events happen.
--
-- The function bake just calls step repeatedly. It's used for the
-- examples below. Don't modify it.
--
-- Examples:
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake]  ==>  Finished
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake,AddSugar,Mix]  ==> Finished
--   bake [AddFlour]  ==>  Error
--   bake [AddEggs,AddFlour,Mix]  ==>  Error

data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq, Show)

data State = Start | MissingBaking | MissingMixing | MissingFlourAndSugar | MissingFlour | MissingSugar | Error | Finished
  deriving (Eq, Show)

step Error _ = Error
step Finished _ = Finished
step Start AddEggs = MissingFlourAndSugar
step MissingFlourAndSugar a = case a of
  AddSugar -> MissingFlour
  AddFlour -> MissingSugar
  _ -> Error
step MissingFlour AddFlour = MissingMixing
step MissingSugar AddSugar = MissingMixing
step MissingMixing Mix = MissingBaking
step MissingBaking Bake = Finished
step _ _ = Error

{-
data State = Start | Eggs | EggsFlour | EggsSugar | AllIngredients | Mixed | Error | Finished
  deriving (Eq,Show)

step :: State -> Event -> State
step Start          AddEggs  = Eggs
step Eggs           AddFlour = EggsFlour
step Eggs           AddSugar = EggsSugar
step EggsFlour      AddSugar = AllIngredients
step EggsSugar      AddFlour = AllIngredients
step AllIngredients Mix      = Mixed
step Mixed          Bake     = Finished
step Finished       _        = Finished
step _              _        = Error

pretty much the same thing, but this makes sense, although mine is a little more readable ("What should I put next" is more important than "What do I have")
-}

-- do not edit this
bake :: [Event] -> State
bake events = go Start events
  where
    go state [] = state
    go state (e : es) = go (step state e) es

------------------------------------------------------------------------------
-- Ex 4: remember how the average function from Set4 couldn't really
-- work on empty lists? Now we can reimplement average for NonEmpty
-- lists and avoid the edge case.
--
-- PS. The Data.List.NonEmpty type has been imported for you
--
-- Examples:
--   average (1.0 :| [])  ==>  1.0
--   average (1.0 :| [2.0,3.0])  ==>  2.0

average :: (Fractional a) => NonEmpty a -> a
average (x :| xs) = sum (x : xs) / fromIntegral (length (x : xs))

------------------------------------------------------------------------------
-- Ex 5: reverse a NonEmpty list.
--
-- PS. The Data.List.NonEmpty type has been imported for you

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (x :| xs) = head reversed :| tail reversed
  where
    reversed = reverse (x : xs)

------------------------------------------------------------------------------
-- Ex 6: implement Semigroup instances for the Distance, Time and
-- Velocity types from exercise 1. The instances should perform
-- addition.
--
-- When you've defined the instances you can do things like this:
--
-- velocity (Distance 50 <> Distance 10) (Time 1 <> Time 2)
--    ==> Velocity 20

instance Semigroup Distance where
  Distance x <> Distance x' = Distance (x + x')

instance Semigroup Time where
  Time x <> Time x' = Time (x + x')

instance Semigroup Velocity where
  Velocity x <> Velocity x' = Velocity (x + x')

------------------------------------------------------------------------------
-- Ex 7: implement a Monoid instance for the Set type from exercise 2.
-- The (<>) operation should be the union of sets.
--
-- What's the right definition for mempty?
--
-- What are the class constraints for the instances?

instance (Ord a) => Semigroup (Set a) where
  Set xs <> s@(Set a) = foldr go s xs
    where
      go v s = add v s

-- wow I was dumb here
-- foldr add s xs

instance (Ord a) => Monoid (Set a) where
  mempty = emptySet

-- Set [] would be better

------------------------------------------------------------------------------
-- Ex 8: below you'll find two different ways of representing
-- calculator operations. The type Operation1 is a closed abstraction,
-- while the class Operation2 is an open abstraction.
--
-- Your task is to add:
--  * a multiplication case to Operation1 and Operation2
--    (named Multiply1 and Multiply2, respectively)
--  * functions show1 and show2 that render values of
--    Operation1 and Operation2 to strings
--
-- Examples:
--   compute1 (Multiply1 2 3) ==> 6
--   compute2 (Multiply2 2 3) ==> 6
--   show1 (Add1 2 3) ==> "2+3"
--   show1 (Multiply1 4 5) ==> "4*5"
--   show2 (Subtract2 2 3) ==> "2-3"
--   show2 (Multiply2 4 5) ==> "4*5"

data Operation1
  = Add1 Int Int
  | Subtract1 Int Int
  | Multiply1 Int Int
  deriving (Show)

compute1 :: Operation1 -> Int
compute1 (Add1 i j) = i + j
compute1 (Subtract1 i j) = i - j
compute1 (Multiply1 i j) = i * j

show1 :: Operation1 -> String
show1 (Add1 a b) = show a ++ "+" ++ show b
show1 (Subtract1 a b) = show a ++ "-" ++ show b
show1 (Multiply1 a b) = show a ++ "*" ++ show b

data Add2 = Add2 Int Int
  deriving (Show)

data Subtract2 = Subtract2 Int Int
  deriving (Show)

data Multiply2 = Multiply2 Int Int
  deriving (Show)

class Operation2 op where
  compute2 :: op -> Int
  show2 :: op -> String

instance Operation2 Add2 where
  compute2 (Add2 i j) = i + j
  show2 (Add2 a b) = show a ++ "+" ++ show b

instance Operation2 Subtract2 where
  compute2 (Subtract2 i j) = i - j
  show2 (Subtract2 a b) = show a ++ "-" ++ show b

instance Operation2 Multiply2 where
  compute2 (Multiply2 i j) = i * j
  show2 (Multiply2 a b) = show a ++ "*" ++ show b

------------------------------------------------------------------------------
-- Ex 9: validating passwords. Below you'll find a type
-- PasswordRequirement describing possible requirements for passwords.
--
-- Implement the function passwordAllowed that checks whether a
-- password is allowed.
--
-- Examples:
--   passwordAllowed "short" (MinimumLength 8) ==> False
--   passwordAllowed "veryLongPassword" (MinimumLength 8) ==> True
--   passwordAllowed "password" (ContainsSome "0123456789") ==> False
--   passwordAllowed "p4ssword" (ContainsSome "0123456789") ==> True
--   passwordAllowed "password" (DoesNotContain "0123456789") ==> True
--   passwordAllowed "p4ssword" (DoesNotContain "0123456789") ==> False
--   passwordAllowed "p4ssword" (And (ContainsSome "1234") (MinimumLength 5)) ==> True
--   passwordAllowed "p4ss" (And (ContainsSome "1234") (MinimumLength 5)) ==> False
--   passwordAllowed "p4ss" (Or (ContainsSome "1234") (MinimumLength 5)) ==> True

data PasswordRequirement
  = MinimumLength Int
  | ContainsSome String -- contains at least one of given characters
  | DoesNotContain String -- does not contain any of the given characters
  | And PasswordRequirement PasswordRequirement -- and'ing two requirements
  | Or PasswordRequirement PasswordRequirement -- or'ing
  deriving (Show)

passwordAllowed :: String -> PasswordRequirement -> Bool
passwordAllowed p (And l r) = passwordAllowed p l && passwordAllowed p r
passwordAllowed p (Or l r) = passwordAllowed p l || passwordAllowed p r
passwordAllowed p (MinimumLength x) = length p >= x
passwordAllowed [] (ContainsSome x) = False
passwordAllowed (p : ps) (ContainsSome x) = elem p x || passwordAllowed ps (ContainsSome x)
passwordAllowed [] (DoesNotContain x) = True
passwordAllowed (p : ps) (DoesNotContain x) = notElem p x && passwordAllowed ps (DoesNotContain x)

------------------------------------------------------------------------------
-- Ex 10: a DSL for simple arithmetic expressions with addition and
-- multiplication. Define the type Arithmetic so that it can express
-- expressions like this. Define the functions literal and operation
-- for creating Arithmetic values.
--
-- Define two interpreters for Arithmetic: evaluate should compute the
-- expression, and render should show the expression as a string.
--
-- Examples:
--   evaluate (literal 3) ==> 3
--   render   (literal 3) ==> "3"
--   evaluate (operation "+" (literal 3) (literal 4)) ==> 7
--   render   (operation "+" (literal 3) (literal 4)) ==> "(3+4)"
--   evaluate (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> 6
--   render   (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> "(3*(1+1))"
--

data Arithmetic = Literal Integer | Op String Arithmetic Arithmetic
  deriving (Show)

literal :: Integer -> Arithmetic
literal x = Literal x

operation :: String -> Arithmetic -> Arithmetic -> Arithmetic
operation op a b = Op op a b

evaluate :: Arithmetic -> Integer
evaluate (Literal x) = x
evaluate (Op op a b) = case op of
  "+" -> evaluate a + evaluate b
  "*" -> evaluate a * evaluate b

render :: Arithmetic -> String
render (Literal x) = show x
render (Op op a b) = "(" ++ render a ++ op ++ render b ++ ")"
