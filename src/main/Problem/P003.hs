module Problem.P003 where
import Data.Time
import Data.List

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

primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Integer -> Bool
isPrime n | n < 2 = False
          | otherwise = all ((/= 0) . mod n) $ ps
  where ps = takeWhile ((<= n) . (^(2 :: Integer))) primes

primeFactors :: Integer -> [Integer]
primeFactors n | n < 2 = []
               | otherwise = case find ((== 0) . mod n) ps of
                                  Just p -> p : (primeFactors $ n `div` p)
                                  Nothing -> [n]
  where ps = takeWhile ((<= n) . (^(2 :: Integer))) primes

solve :: Integer -> Integer
solve = last . primeFactors

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 600851475143
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
