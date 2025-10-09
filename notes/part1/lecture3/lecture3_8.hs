-- Now for list building:
descend 0 = []
descend n = n : descend (n-1)

split c [] = []
split c xs = start : split c (drop 1 rest)
  where start = takeWhile (/= c) xs
        rest = dropWhile (/= c) xs

-- it also shows that you can _arg instead of only _
-- Apparently tco isn't used in haskell that much, and ghc can spot and do that for you sometimes.
--
-- [f x | x <- lis, p x]
-- map f (filter p lis)
-- list comprehension my beloved
--
-- wow that's funny
-- [ first ++ " " ++ last | first <- ["John", "Mary"], last <- ["Smith","Cooper"] ]
-- ==> ["John Smith","John Cooper","Mary Smith","Mary Cooper"]
--
-- wow that's funny as well
-- [ reversed | word <- ["this","is","a","string"], let reversed = reverse word ]
-- ==> ["siht","si","a","gnirts"]
--
-- wow so powerful
-- firstLetters string = [ char | (char:_) <- words string ]
--
-- anything in !#$%&*+./<=>?@\^|-~ can be an operator
--
-- _holes are a compiler thing that exists and can be useful
