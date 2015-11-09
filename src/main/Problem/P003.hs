module Problem.P003 where
import Data.Time
import Common.Arithmetic

{-
- Largest prime factor
- https://projecteuler.net/problem=3
-}

{-
- [方針1]
- 素因数分解を行い、出てきた素因数の最大値を答えとする。
-
- [結果]
- 6857
- time:0.007092s
-}

solve :: Integer -> Integer
solve = last . primeFactors

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 600851475143
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
