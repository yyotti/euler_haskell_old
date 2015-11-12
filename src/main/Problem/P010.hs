module Problem.P010 where
import Common.Arithmetic

{-
- Summation of primes
- https://projecteuler.net/problem=10
-}

{-
- [方針1]
- 素数列の小さい方から指定された最大値まで取り出して和をとる
-
- [結果]
- 142913828922
- time:1.74288s
-}
solve :: Integral a => a -> Integer
solve n = sum $ takeWhile (<= fromIntegral n) (primes :: [Integer])
