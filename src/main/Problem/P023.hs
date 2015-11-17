module Problem.P023 where
import Common.Arithmetic
import Data.List
import Data.Set hiding (map, filter, foldl')

{-
- Non-abundant sums
- https://projecteuler.net/problem=23
-}

{-
- [方針1]
- 指定された数以下の過剰数を求めて、組み合わせで作れる数字を消していく。
-
- [結果]
- 4179871
- time:11.356423s
-}

primeFactorsExp :: Integral a => a -> [(a, Int)]
primeFactorsExp n = map (\ ls -> (head ls, length ls)) $ group $ primeFactors n

sumProperDivisors :: Integral a => a -> a
sumProperDivisors n = (product $ map (\ (p, e) -> sum $ map (p^) [0..e]) $ primeFactorsExp n) - n

isAbundantNumber :: Integral a => a -> Bool
isAbundantNumber n = sumProperDivisors n > n

abundantNumbers :: Integral a => [a]
abundantNumbers = filter isAbundantNumber [1..]

solve :: Integral a => a -> Integer
solve n = toInteger $ sum $ toList $ foldl' (\ set x -> difference set $ sums x) initialSet nums
  where nums = takeWhile (<= n) abundantNumbers
        initialSet = fromList [1..n]
        sums x = fromList $ takeWhile (<= n) $ map (+ x) $ dropWhile (< x) nums
