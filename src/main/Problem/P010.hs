module Problem.P010 where
import Data.Time
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
- time:36.233071s
-}
solve :: Integer -> Integer
solve n = sum $ takeWhile (<= n) primes

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 2000000
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
