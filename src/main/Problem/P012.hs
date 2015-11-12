module Problem.P012 where
import Data.List
import Common.Arithmetic

{-
- [Problem name]
- https://projecteuler.net/problem=12
-}

{-
- [方針1]
- 三角数の数列をt(n)とすると、1からnまでの自然数の和なので
-   t(n) = n(n + 1)/2   (n >= 1)
- である。
-
- 約数の個数m(n)は、nを素因数分解した結果が
-   n = p1^e1 * p2^e2 * ... * pk^ek   (pkは素数、ekは自然数)
- とすれば、
-   m(n) = Π(i = 1 → k) { ei + 1 }
- で算出できる。
-
- [結果]
- 76576500
- time:10.200946s
-}

triangleNumbers :: Integral a => [a]
triangleNumbers = map t [1..]
  where t n = n * (n + 1) `div` 2

factorsCount :: Integral a => a -> a
factorsCount n = product $ map ((+ 1) . fromIntegral . length) $ groupBy (==) $ primeFactors n

solve :: Integral a => a -> Integer
solve n = case find ((> n) . factorsCount) triangleNumbers of
               Just k -> toInteger k
               _ -> 0
