module Problem.P041 where
import Common.Arithmetic
import Common.Util
import Data.List

{-
- Pandigital prime
- https://projecteuler.net/problem=41
-}

{-
- [方針1]
- 素数の中からn桁パンデジタルのものを抽出し、最大値を得る。
-
- [結果]
- 時間がかかりすぎるので中断
-}

isPandigitalN :: Integral a => a -> Bool
isPandigitalN x = [1..length ds] == sort ds
  where ds = digits x

solve :: Integer
solve = toInteger $ maximum $ filter isPandigitalN $ takeWhile (< 10^9) primes
