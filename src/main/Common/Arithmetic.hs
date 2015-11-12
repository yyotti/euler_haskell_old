module Common.Arithmetic where
import Data.List

{-# SPECIALIZE fib :: [Int] #-}
{-# SPECIALIZE fib :: [Integer] #-}
fib :: Integral a => [a]
fib = 0 : 1 : (zipWith (+) fib $ tail fib)

primes :: Integral a => [a]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Integral a => a -> Bool
isPrime n | n < 2 = False
          | otherwise = all ((/= 0) . mod n) $ ps
  where ps = takeWhile ((<= n) . (^2)) primes

primeFactors :: Integral a => a -> [a]
primeFactors n | n < 2 = []
               | otherwise = case find ((== 0) . mod n) ps of
                                  Just p -> p : (primeFactors $ n `div` p)
                                  Nothing -> [n]
  where ps = takeWhile ((<= n) . (^2)) primes
