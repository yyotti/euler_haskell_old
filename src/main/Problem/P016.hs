module Problem.P016 where
import Common.Util

{-
- Power digit sum
- https://projecteuler.net/problem=16
-}

{-
- [方針1]
- n^e を計算し、各桁の和を求める
-
- [結果]
- 1366
- time:0.000298s
-}
solve :: Integral a => a -> a -> Integer
solve = (toInteger .) . (sum .) . (digits .) . (^) . fromIntegral
