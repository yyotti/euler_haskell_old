module Problem.P027 where
import Common.Arithmetic
import Data.List
import Data.Ord

{-
- Quadratic primes
- https://projecteuler.net/problem=27
-}

{-
- [方針1]
- ストレートに、指定された値の範囲内で探す。
-
- [結果]
- 中断(数分かかっても終了しない)
-}

primesCount :: Integral a => a -> a -> Int
primesCount a b = length $ takeWhile isPrime nums
  where nums = map (\ n -> n^2 + a * n + b) [0..1000]

solve :: Integral a => a -> Integer
solve m = toInteger $ product $ maximumBy (comparing pc) ab
  where pc [a, b] = primesCount a b
        ab = sequence [[(-m)..m], [(-m)..m]]
