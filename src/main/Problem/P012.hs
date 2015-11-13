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

{-
- [方針2]
- 下記の定理と補題を使ってみる。
-
- (定理)
- k,lを自然数とし、kとlが互いに素である場合、
-   m(k*l) = m(k) * m(l)
- である。
-
- (補題)
- kとlを互いに素な自然数、rをkの約数とする。このとき、(k/r)とlは互いに素である。
-
- (補題の証明)
- kとlは互いに素なので、それぞれの約数を列挙しても1以外の共通の約数は表れない。
- (k/r)は、kの約数からrを除いた約数を掛け合わせた数になるが、もともとkとlには1以外の公約数が無い
- ため、kの約数からrが除かれてもやはり公約数は1のみである。
-
- いま、
-   t(n) = n(n + 1)/2
- であるが、nと(n + 1)は連続する2つの自然数なので、互いに素である。
- nと(n + 1)のいずれかは偶数なので、分母の2で割り切れる(つまり約数に2をもつ)。
-   nが偶数のとき・・・(n/2)が整数になるが、補題より(n/2)と(n + 1)は互いに素である。
-   (n + 1)が偶数のとき・・・(n + 1)/2が整数になるが、補題よりnと(n + 1)/2は互いに素である。
-
- よって、
-   nが偶数のとき・・・m(t(n)) = m((n/2)*(n + 1)) = m(n/2) * m(n + 1)
-   nが奇数のとき・・・m(t(n)) = m(n*((n + 1)/2)) = m(n) * m((n + 1)/2)
- となる。
-
- [結果]
- 76576500
- time:0.604879s
-}

triangleNumbers :: Integral a => [(a, a)]
triangleNumbers = map (\ n -> (t (fromIntegral n), m (fromIntegral n))) [1..]
  where t n = n * (n + 1) `div` 2
        m n | even n = factorsCount (n `div` 2) * factorsCount (n + 1)
            | otherwise = factorsCount n * factorsCount ((n + 1) `div` 2)

factorsCount :: Integral a => a -> a
factorsCount n = product $ map ((+ 1) . fromIntegral . length) $ groupBy (==) $ primeFactors n

solve :: Integral a => a -> Integer
solve n = case find ((> n) . snd) triangleNumbers of
               Just (t, _) -> toInteger t
               _ -> 0
