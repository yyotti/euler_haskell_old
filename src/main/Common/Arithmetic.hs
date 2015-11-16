module Common.Arithmetic where
import Data.List

{-# SPECIALIZE fib :: [Int] #-}
{-# SPECIALIZE fib :: [Integer] #-}
fib :: Integral a => [a]
fib = 0 : 1 : (zipWith (+) fib $ tail fib)

{-# SPECIALIZE primes :: [Int] #-}
{-# SPECIALIZE primes :: [Integer] #-}
primes :: Integral a => [a]
primes = 2 : filter isPrime [3, 5 ..]

{-# SPECIALIZE isPrime :: Int -> Bool #-}
{-# SPECIALIZE isPrime :: Integer -> Bool #-}
isPrime :: Integral a => a -> Bool
isPrime n | n < 2 = False
          | otherwise = all ((/= 0) . mod n) $ ps
  where ps = takeWhile ((<= n) . (^2)) primes

{-# SPECIALIZE primeFactors :: Int -> [Int] #-}
{-# SPECIALIZE primeFactors :: Integer -> [Integer] #-}
primeFactors :: Integral a => a -> [a]
primeFactors n | n < 2 = []
               | otherwise = case find ((== 0) . mod n) ps of
                                  Just p -> p : (primeFactors $ n `div` p)
                                  Nothing -> [n]
  where ps = takeWhile ((<= n) . (^2)) primes

fact :: Integral a => a -> a
fact 0 = 1
fact n | n < 0 = 0
       | otherwise = n * fact (n - 1)
