module Problem.P021 where
import Data.List
import Common.Arithmetic

{-
- Amicable numbers
- https://projecteuler.net/problem=21
-}

{-
- [方針1]
- 指定された数未満のd(n)を求め、それをmとする。
-   d(m) = n  (m ≠ n)
- である場合をカウントしていく。
- ただし重複が考えられるので、最後に2で割らなければならない。
- また、場合によっては d(n) が最大値を超える可能性があるので注意する。
-
- nが
-   n = p1^e1 * p2^e2 * ... * pk^ek  (pk は素数、ekは自然数)
- と素因数分解できるとき、nの約数の和は
-   (1 + p1 + p1^2 + ... + p1^e1)*(1 + p2 + p2^2 + ... + p2^e2)
- で表すことができる。
-
- [結果]
- 31626
- time:0.418841s
-}

primeFactorsCount :: Integral a => a -> [(a, Int)]
primeFactorsCount = map toTuple . group . primeFactors
  where toTuple ls = (head ls, length ls)

factorsSum :: Integral a => a -> a
factorsSum = product . map s . primeFactorsCount
  where s (p, e) = sum $ map (p^) [0..e]

amicableNum :: Integral a => a -> a -> [a]
amicableNum m n | d2 == n && d1 < m && d1 /= n = [n, d1]
                | otherwise = []
  where d1 = factorsSum n - n
        d2 = factorsSum d1 - d1

solve :: Integral a => a -> Integer
solve n = toInteger $ (sum . concatMap (amicableNum n)) [1..n-1] `div` 2
