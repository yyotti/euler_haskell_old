module Problem.P045 where
import Common.Arithmetic
import Data.List
import Data.Maybe
import Control.Monad

{-
- Triangular, pentagonal, and hexagonal
- https://projecteuler.net/problem=45
-}

{-
- [方針1]
- 三角数、五角数、六角数の列をそれぞれ生成し、指定の数以上になるよう
- 調整した後で、等しくなるものを探していく
-
- [結果]
- 1533776805
- time:0.021379s
-
- [コミット]
- 2f01142
-}

{-
- [方針2]
- 六角数が一番速く大きくなっていくので、六角数を基準に探す。
- 探す際も五角数か否かを判定してから三角数か否かを判定する。
-
- [結果]
- 1533776805
- time:0.015032s
-
- [コミット]
- 13e3e8d
-}

{-
- [方針3]
- P044でやったように、五角数か否かの判定は
-   P(n) = n(3n-1)/2 = m
- として、
-   3n^2 - n - 2m = 0
- が整数解を持てばよい。それには判別式をdとして
-   d = 1 - 4*3*(-2m) = 1 + 24m
-   n = (1 + √d)/6
- が整数になれば五角数と判定できる。
-
- 同様に、三角数か否かの判定は
-   T(n) = n(n+1)/2 = m
- として、
-   n^2 + n - 2m = 0
- が整数解を持てばよい。それには判別式をdとして
-   d = 1 - 4*1*(-2m) = 1 + 8m
-   n = (1 + √d)/2
- が整数になれば三角数と判定できる。
-
- [結果]
- 1533776805
- time:0.007307s
-}

hexagonals :: Integral a => [a]
hexagonals = 1 : zipWith (+) hexagonals [5,9..]

isPentagonal :: Integral a => a -> Bool
isPentagonal = isInteger . (/6) . (1+) . sqrt . (1+) . (24*) . fromIntegral

isTriangle :: Integral a => a -> Bool
isTriangle = isInteger . (/2) . (1+) . sqrt . (1+) . (8*) . fromIntegral

solve :: Int -> Integer
solve = toInteger . fromJust . find (ap ((&&) . isTriangle) isPentagonal) . flip dropWhile hexagonals . (>=)
