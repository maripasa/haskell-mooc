repeatString n str = repeatHelper n str ""

{-
repeatHelper n str result = if (n==0)
                            then result
                            else repeatHelper (n-1) str (result++str)

or, the prettier:

repeatHelper 0 _ result   = result
repeatHelper n str result = repeatHelper (n-1) str (result++str)

or even:

-}

repeatHelper n str result
  | n == 0 = result
  | otherwise = repeatHelper (n - 1) str (result ++ str)

fib :: Integer -> Integer
fib = fib' 0 1

-- here the lsp suggested a reduction, which is funny

fib' :: Integer -> Integer -> Integer -> Integer
fib' a b 1 = b
fib' a b n = fib' b (a + b) (n - 1)

describe :: Int -> String
describe n
  | n == 2 = "Two"
  | even n = "Even"
  | n == 3 = "Three"
  | n > 100 = "Big!!"
  | otherwise = "The number " ++ show n

-- Here format - again from the lsp - is seen fixing the comparisons - which were x==0 instead of x == 0 -, but not making new lines equal.

factorial :: Integer -> Integer
factorial n
  | n<0 = -1
  | n==0 = 1
  | otherwise = factorial n * factorial (n-1)

guessAge :: String -> Int -> String
guessAge "Griselda" age
  | age < 47 = "Too low!"
  | age > 47 = "Too high!"
  | otherwise = "Correct!"
guessAge "Hansel" age
  | age < 12 = "Too low!"
  | age > 12 = "Too high!"
  | otherwise = "Correct!"
guessAge name age = "No one there."

f xs = take 2 xs ++ drop 4 xs
g xs = tail xs ++ [head xs]

f2 xs ys = [head xs, head ys]
g2 = f2 "Moi"

-- of course, you can do partials without notation
