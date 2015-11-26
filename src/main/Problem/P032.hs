module Problem.P032 where
import Data.List
import Common.Util

{-
- Pandigital products
- https://projecteuler.net/problem=32
-}

{-
- [方針1]
- 掛けられる数/掛ける数/積が1から9のパンデジタルになるということは、それぞれの
- 桁数をd1,d2,d3とすると、
-   d1 + d2 + d3 = 9
- ということである。
- また、1通り以上の掛けられる数/掛ける数/積の組み合わせを持つ場合は1回だけ数え
- ればよいので、d1 <= d2 <= d3 としてよい。
-
- よって、(d1,d2,d3)の組で考えられるのは
-   (1,1,7), (1,2,6), (1,3,5), (1,4,4),
-   (2,2,5), (2,3,4), (3,3,3)
- である。
-
- 掛け算を行うと、桁数の関係として
-   d3 - 1 <= d1 + d2 <= d3
- となるはずである。これを満たす組み合わせは
-   (d1,d2,d3) = (1,4,4), (2,2,5), (2,3,4)
- のみである。
-
- [結果]
- 45228
- time:0.192911s
-}
solve :: Integer
solve = (sum . nub . map head . filter isPandigital . concatMap numPairs) [[1, 4], [2, 2], [2, 3]]
  where
    numPairs ls = map (\ [a, b] -> [a * b, a, b]) $ sequence $ map range ls
    range n = [10^(n-1) .. 10^n-1]
