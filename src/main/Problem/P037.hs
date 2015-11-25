module Problem.P037 where
import Common.Util
import Control.Monad
import Data.List
import Common.Arithmetic

{-
- Truncatable primes
- https://projecteuler.net/problem=37
-}

{-
- [方針1]
- 問題によると11個しかないということなので、素数を順に調べていって
- 切り詰め素数が11個になったところでやめる。
-
- [結果]
- 748317
- time:0.811496s
-}

trimr :: Integral a => a -> a
trimr = flip div 10

triml :: Integral a => a -> a
-- triml n = n `mod` 10^e
--   where e = (length $ digits n) - 1
-- ↑をポイントフリーで書くと↓になるが、やりすぎか？
triml = ap (flip mod . (10^) . e) id
  where e = flip (-) 1 . length . digits

truncates :: Integral a => a -> [a]
truncates = nub . ap ((++) . rs) ls
  where rs = takeWhile (> 0) . iterate trimr
        ls = takeWhile (> 0) . iterate triml

isTruncatablePrime :: Integral a => a -> Bool
isTruncatablePrime = ap ((&&) . (>= 10)) (all isPrime . truncates)

solve :: Int -> Integer
solve = toInteger . sum . flip take truncatablePrimes . min 11
  where truncatablePrimes = filter isTruncatablePrime primes
