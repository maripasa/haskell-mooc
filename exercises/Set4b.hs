-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold. The function countNothings from
-- the course material can be implemented using foldr. Your task is to
-- define countHelper so that the following definition of countNothings
-- works.
--
-- Hint: You can start by trying to add a type signature for countHelper.
--
-- Challenge: look up the maybe function and use it in countHelper.
--
-- Examples:
--   countNothings []  ==>  0
--   countNothings [Just 1, Nothing, Just 3, Nothing]  ==>  2

countNothings :: [Maybe a] -> Int
countNothings xs = foldr countHelper 0 xs

countHelper :: (Num b) => Maybe a -> b -> b
countHelper item counter = maybe (counter + 1) (const counter) item

-- His was weirder:
--
-- countHelper :: Maybe a -> Int -> Int
-- countHelper Nothing n = n+1
-- countHelper (Just _) n = n
------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x : xs) = foldr maxHelper x xs

maxHelper :: (Ord a) => a -> a -> a
maxHelper num maxi = max num maxi

-- Just realized... The lsp didn't show the redundancy.
-- maxHelper = max
------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
--   sumAndLength []             ==>  (0.0,0)
--   sumAndLength [1.0,2.0,4.0]  ==>  (7.0,3)

sumAndLength :: [Double] -> (Double, Int)
sumAndLength xs = foldr slHelper slStart xs

slStart = (0.0, 0)

slHelper :: (Num a, Num b) => a -> (a, b) -> (a, b)
slHelper value (sum, count) = (sum + value, count + 1)

-- same but with less clear notation. "Big names have to be written out!" and "Let's abbreviate this 5 letter word!"

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

myConcat :: [[a]] -> [a]
myConcat xs = foldr concatHelper concatStart xs

concatStart = []

concatHelper list cur = list ++ cur

-- AGAIN.
-- concatHelper = (++)

------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

largest :: [Int] -> [Int]
largest xs = foldr largestHelper [] xs

largestHelper num [] = [num]
largestHelper num (x : xs)
  | num > x = [num]
  | num == x = x : xs ++ [x]
  -- Here I could have written num == x = num:x:xs or even x:x:xs
  | otherwise = x : xs

------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
--
-- Examples:
--   myHead []  ==>  Nothing
--   myHead [1,2,3]  ==>  Just 1

myHead :: [a] -> Maybe a
myHead xs = foldr headHelper Nothing xs

headHelper :: a -> p -> Maybe a
headHelper item _ = Just item

------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
--
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

myLast :: [a] -> Maybe a
myLast xs = foldr lastHelper Nothing xs

lastHelper :: a -> Maybe a -> Maybe a
lastHelper item current = case current of
  Nothing -> Just item
  (Just _) -> current

-- his was pretty similar
