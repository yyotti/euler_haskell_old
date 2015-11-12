module Problem.P003 where
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
- time:0.006323s
-}

solve :: Integer -> Integer
solve = last . primeFactors
