module Problem.P044 where
import Data.List
import Data.Maybe
import Common.Arithmetic

{-
- Pentagon numbers
- https://projecteuler.net/problem=44
-}

{-
- [方針1]
- 五角数の列を小さい方から順に調べ、最初に見つけた(j,k)を採用する。
-
- [結果]
- 5482660
- time:42.819109s
-
- [コミット]
- 5c842fc
-}

{-
- [方針2]
- 整数mが五角数か否かを調べる関数が遅すぎるので工夫する。
-
-   P(n) = n(3n-1)/2 = m
- として、
-   3n^2 - n - 2m = 0
- が整数解を持てばよい。それには判別式をdとして
-   d = 1 - 4*3*(-2m) = 1 + 24m
-   n = (1 + √d)/6
- が整数になれば五角数と判定できる。
-
- [結果]
- 5482660
- time:0.353837s
-}

pentagonals :: Integral a => [a]
pentagonals = 1 : zipWith (+) pentagonals [4,7..]

isPentagonNumber :: Integral a => a -> Bool
isPentagonNumber = isInteger . (/6) . (1+) . sqrt . fromIntegral . (1+) . (24*)

solve :: Integer
solve = (toInteger . minimum . fromJust . find ((> 0) . length) . map pentagonalPairs) [1..]
  where pentagonalPairs n = map (\ v -> h - v) $ filter (\ v -> isPentagonNumber (h + v) && isPentagonNumber (h - v)) ts
          where (h:ts) = (reverse . take n) pentagonals
