{-
 class  (Eq a) => Ord a  where
  compare              :: a -> a -> Ordering
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min             :: a -> a -> a

  compare x y | x == y    = EQ
              | x <= y    = LT
              | otherwise = GT

  x <= y  = compare x y /= GT
  x <  y  = compare x y == LT
  x >= y  = compare x y /= LT
  x >  y  = compare x y == GT

  max x y | x <= y    =  y
          | otherwise =  x
  min x y | x <= y    =  x
          | otherwise =  y

https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#t:Ord Minimal implementation is <= or compare
-}

class Example a where
  example :: a           -- the main example for the type `a`
  examples :: [a]        -- a short list of examples
  examples = [example]   -- ...defaulting to just the main example

instance Example Int where
  example = 1
  examples = [0,1,2]

instance Example Bool where
  example = True




class Size a where
  empty :: a
  size :: a -> Int
  sameSize :: a -> a -> Bool

instance Size (Maybe a) where
  empty = Nothing

  size Nothing = 0
  size (Just a) = 1

  sameSize x y = size x == size y

instance Size [a] where
  empty = []
  size xs = length xs
  -- here it wants to do size = length
  sameSize x y = size x == size y

-- if it's hard to see another definition other then the default one, just put it outside
class Combine a where
  combine :: a -> a -> a

combine3 :: Combine a => a -> a -> a -> a
combine3 x y z = combine x (combine y z)

data IntPair = IntPair Int Int
  deriving Show

instance Eq IntPair where
  IntPair a1 a2 == IntPair b1 b2  =  a1==b1 && a2==b2

instance Ord IntPair where
  IntPair a1 a2 <= IntPair b1 b2
     | a1<b1     = True
     | a1>b1     = False
     | otherwise = a2<=b2

-- You will be deriving Show and Eq, probably not Ord
-- >info Num in ghci for fast doc


-- instance Eq Maybe where (maybe needs a type)
-- instance Eq (a,a) where (overlapping instances)
-- instance Eq (Maybe Int) (overlapping instances)
-- instance Eq (a,b) where (overlapping instances too???)

-- I'll check in the telegram
