x = (1,2)

--fst :: (a, b) -> a
--snd :: (a, b) -> b

-- Finally, we have reduce
--

sumNumbers xs = foldr (+) 0 xs

-- WOW the lsp recognizes sum!
-- Use sum Found: foldr (+) 0  Why not:   sum

map g xs = foldr helper [] xs
  where helper y ys = g y : ys

-- here it does not see map, but it sees xs can be removed
--
-- Also type classes
