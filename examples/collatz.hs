module Collatz where
-- one step of the Collatz sequence
step :: Integer -> Integer
step x = if even x then down else up
  where down = div x 2
        up = 3*x+1

-- collatz x computes how many steps it takes for the collatz sequence to reach 1 when starting from x
collatz :: Integer -> Integer
collatz 1 = 0
collatz x = 1 + collatz (step x)

-- Longest finds the number with the Longest Collatz sequence for initial values between 0 and upperBound
longest :: Integer -> Integer
longest = longest' 0 0

-- helper function for Longest
longest' :: Integer -> Integer -> Integer -> Integer
-- end of recursion, return longest length found
longest' number _ 0 = number
-- recursion step: check if n has a longer Collatz sequence than the current know longest
longest' number maxlength n =
  if len > maxlength
  then longest' n len (n-1)
  else longest' number maxlength (n-1)
  where len = collatz n

