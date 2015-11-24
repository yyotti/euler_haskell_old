module Problem.P035 where
import Common.Arithmetic
import Common.Util

{-
- Circular primes
- https://projecteuler.net/problem=35
-}

{-
- [方針1]
- 100万未満の素数を列挙して、巡回素数かどうか順に調べていく。
-
- [結果]
- 55
- time:0.755195s
-}

cycles :: Integral a => a -> [a]
cycles n = cycles' n e
  where e = length $ digits n
        cycles' _ 0 = []
        cycles' n k = n : cycles' (m * 10 + d) (k-1)
          where (d, m) = n `divMod` (10^(e-1))

solve :: Int -> Integer
solve = toInteger . length . filter (all isPrime . cycles) . flip takeWhile primes . (>=)
