import Data.Function ((&))
import Data.List (tails)

-- Nothing new really, just an introduction to hi-order functions
-- Some fun stuff tho
-- wrapJust xs = map Just xs
-- or
wrapJust = map Just
-- partial aplication shenanigans
-- infix notations
-- infix vs prefix, and how to change in between
--
(%>) :: a -> (a -> b) -> b
x %> f = f x
{-# INLINE (%>) #-}
-- Fun stuff to compiler just replace instead of calling the function, so no function call (GHC pragma)
-- tadaa, piping!
infixl 0 %>
-- left associative and min precedence

result :: Int
result =
  [1..]
    %> filter even
    %> sum
    %> (*2)

-- This already exists in Data.Function:

result2 :: Int
result2 =
  [1..]
    & filter even
    & sum
    & (*2)

{-
-- | '&' is a reverse application operator.  This provides notational
-- convenience.  Its precedence is one higher than that of the forward
-- application operator '$', which allows '&' to be nested in '$'.
--
--
-- This is a version of @'flip' 'id'@, where 'id' is specialized from @a -> a@ to @(a -> b) -> (a -> b)@
-- which by the associativity of @(->)@ is @(a -> b) -> a -> b@.
-- flipping this yields @a -> (a -> b) -> b@ which is the type signature of '&'
--
-- ==== __Examples__
--
-- >>> 5 & (+1) & show
-- "6"
--
-- >>> sqrt $ [1 / n^2 | n <- [1..1000]] & sum & (*6)
-- 3.1406380562059946
--
-- @since base-4.8.0.0
(&) :: forall r a (b :: TYPE r). a -> (a -> b) -> b
-}

test1 = zipWith (+) [0,2,4] [1,3,5]

-- also

{-
6 `div` 2 ==> div 6 2 ==> 3
(+1) `map` [1,2,3] ==> map (+1) [1,2,3] ==> [2,3,4]
-}

-- also lambdas!
-- (\x -> smth)
-- (\x y -> smth)

-- open way to . and $
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (f.g) x ==> f (g x)

-- for example
{-
let notEmpty x = not (null x)
in filter notEmpty [[1,2,3],[],[4]]
  ==> [[1,2,3],[4]]
-}

-- may be written as

func1 = filter (not . null) [["stuff"]]

-- let's rewrite their example with allf of these
{-
substringsOfLength :: Int -> String -> [String]
substringsOfLength n string = map shorten (tails string)
  where shorten s = take n s

whatFollows :: Char -> Int -> String -> [String]
whatFollows c k string = map tail (filter match (substringsOfLength (k+1) string))
  where match sub = take 1 sub == [c]
-}

whatFollows c k = map tails . filter ((== [c]) . take 1) . map (take (k+1)) . tails
-- takeWhile
-- dropWhile
-- elem
-- id x = x
-- const a b = a


