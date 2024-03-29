module Problem.P006 where

{-
- Sum square difference
- https://projecteuler.net/problem=6
-}

{-
- [方針1]
- 問題に書かれていることをそのままやる。
-
- [結果]
- 25164150
- time:0.000055s
-
- [コミット]
- 4c637f3
-}

{-
- [方針2]
- 2つの数列 a(n), b(n) の一般項を下記のように定義する。
-   a(n) = n^2
-   b(n) = n
- 初項から第n項までの和をそれぞれ Sa(n), Sb(n) とすると、
-   Sa(n) = n(n + 1)(2n + 1)/6
-   Sb(n) = n(n + 1)/2
- である。求める差をS(n)とすると、
-   S(n) = (Sb(n))^2 - Sa(n)
-        = (n(n + 1)/2)^2 - n(n + 1)(2n + 1)/6
-        = ((n^2 + n)/2)^2 - (2n^3 + 3n^2 + n)/6
-        = (n^4 + 2n^3 + n^2)/4 - (2n^3 + 3n^2 + n)/6
-        = (3n^4 + 6n^3 + 3n^2 - (4n^3 + 6n^2 + 2n))/12
-        = (3n^4 + 2n^3 - 3n^2 - 2n)/12
-        = n(3n^3 + 2n^2 - 3n - 2)/12
-        = n(n - 1)(3n^2 + 5n - 2)/12
-        = n(n - 1)(n + 1)(3n + 2)/12
- で求められる。
-
- [結果]
- 25164150
- time:0.000022s
-}

solve :: Integral a => a -> Integer
solve n = toInteger $ n * (n - 1) * (n + 1) * (3 * n + 2) `div` 12
