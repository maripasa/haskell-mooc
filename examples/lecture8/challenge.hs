-- 1  3  4  10
-- 2  5  9  11
-- 6  8  12 15
-- 7  13 14 16

import System.Environment (getArgs)
import Data.List (sort)
import Text.Read (readMaybe)

diag :: Int -> Int -> [(Int, Int)]
diag n k
  | even k = zip [i .. j] (reverse [i .. j])
  | otherwise = zip (reverse [i .. j]) [i .. j]
  where
    j = min k (n - 1)
    i = k - j

solve n = foldr (\(num, coord) last -> replaceAtTable coord num last) table (zip nums coords)
  where
    nums = [1 .. n ^ 2]
    coords = concatMap (diag n) [0 .. 2 * n - 1]
    table = replicate n (replicate n 0)

replaceAtTable :: (Int, Int) -> a -> [[a]] -> [[a]]
replaceAtTable (i, j) value table = replaceAtIndex i newl table
  where
    newl = replaceAtIndex j value (table !! i)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex 0 v (_ : xs) = v : xs
replaceAtIndex i v (x : xs) = x : replaceAtIndex (i - 1) v xs

prettyPrint :: [[Int]] -> String
prettyPrint mat = concatMap (\x -> concatMap fit x ++ "\n") mat
  where
    size = length . show . maximum . map maximum $ mat
    fit = pad (size + 1) . show

pad :: Int -> String -> String
pad size s
  | size <= length s = s
  | otherwise = pad size (" " ++ s)

strToInt :: String -> Maybe Int
strToInt = readMaybe

main = do
  args <- getArgs
  case strToInt . head $ args of
    Just n -> putStr . prettyPrint . solve $ n
    Nothing -> putStr "Invalid argument"
