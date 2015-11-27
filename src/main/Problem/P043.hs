module Problem.P043 where
import Common.Arithmetic
import Common.Util
import Data.List
import Control.Monad

{-
- Sub-string divisibility
- https://projecteuler.net/problem=43
-}

{-
- [方針1]
- 0から9のパンデジタル数を1つ1つ調べていく。
-
- [結果]
- 16695334890
- time:2.219413s
-}

sliding :: Int -> [a] -> [[a]]
sliding _ [] = []
sliding n ls@(_:xs) | length ls <= n = [ls]
                    | otherwise = take n ls : sliding n xs

slidingNums :: Integral a => Int -> [a] -> [a]
slidingNums = (map toNum .) . sliding

isDivisiblePandigital :: Integral a => [a] -> Bool
isDivisiblePandigital = all (== 0) . zipWith (flip mod) primes . (tail . slidingNums 3)

solve :: Integer
solve = (toInteger . sum . map toNum) divisiblePandigitals
  where divisiblePandigitals = (filter (ap ((&&) . ((/= 0) . head)) isDivisiblePandigital) . permutations) [0..9]
