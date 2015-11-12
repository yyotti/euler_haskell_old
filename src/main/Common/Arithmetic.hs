module Common.Arithmetic where
import Data.List

{-# SPECIALIZE fib :: [Int] #-}
{-# SPECIALIZE fib :: [Integer] #-}
fib :: Integral a => [a]
fib = 0 : 1 : (zipWith (+) fib $ tail fib)

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
