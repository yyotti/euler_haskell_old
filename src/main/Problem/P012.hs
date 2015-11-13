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

{-
- [方針3]
- m(t(n))をn=1から順に書き並べていくと、
-   m(t(1)) = m(1*1) = m(1) * m(1) = 1
-   m(t(2)) = m(1*3) = m(1) * m(3) = 2
-   m(t(3)) = m(3*2) = m(3) * m(2) = 4
-   m(t(4)) = m(2*5) = m(2) * m(5) = 4
-   m(t(5)) = m(5*3) = m(5) * m(3) = 4
-   m(t(6)) = m(3*7) = m(3) * m(7) = 4
-   m(t(7)) = m(7*4) = m(7) * m(4) = 6
- と続いていくが、分解された積の片方は1つ前の結果が使える。
- 1つ前の結果 m' を持ち回って計算しようとすると、
-   m(t(1)) = m(1*1) = m(1) * m' = 1   (m'の初期値は1としておく)
-   m(t(2)) = m(1*3) = m' * m(3) = 2, m' = m(3) <- 次のm'として控える
-   m(t(3)) = m(3*2) = m' * m(2) = 4, m' = m(2)
-   m(t(4)) = m(2*5) = m' * m(5) = 4, m' = m(5)
-   m(t(5)) = m(5*3) = m' * m(3) = 4, m' = m(3)
-   m(t(5)) = m(5*3) = m' * m(3) = 4, m' = m(3)
-   m(t(6)) = m(3*7) = m' * m(7) = 4, m' = m(7)
-   m(t(7)) = m(7*4) = m' * m(4) = 6, m' = m(4)
-
- つまり、
-   nが偶数のとき・・・m' * m(n + 1)
-   nが奇数のとき・・・m' * m((n + 1)/2)
- となる。
- これはt(n)からではなくnからt(n)の約数の個数が出せるということなので、先にm(t(n))が500を
- 超えるnを特定し、最後にt(n)を計算すればよい。
-
- [結果]
- 76576500
- time:0.305426s
-}

factorsCount :: Integral a => a -> a
factorsCount = product . map ((+ 1) . fromIntegral . length) . groupBy (==) . primeFactors

factorsCounts :: Integral a => [a]
factorsCounts = map fst $ scanl mt (1, 1) [1..]
  where mt (_, m') n | even n = let m'' = factorsCount (n + 1) in (m' * m'', m'')
                     | otherwise = let m'' = factorsCount ((n + 1) `div` 2) in (m' * m'', m'')

solve :: Integral a => a -> Integer
solve n = case findIndex (> n) factorsCounts of
               Just i -> toInteger $ i * (i + 1) `div` 2
               _ -> 0