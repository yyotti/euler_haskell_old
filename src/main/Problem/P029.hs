module Problem.P029 where
import Data.List as L
import Data.Set as S

{-
- Distinct powers
- https://projecteuler.net/problem=29
-}

{-
- [方針1]
- 全てのパターンを計算して重複を排除する
-
- [結果]
- 9183
- time:0.020457s
-}
solve :: Int -> Integer
solve n = toInteger $ size $ L.foldl' (\ set [x, y] -> S.insert ((toInteger x)^y) set) empty ns
  where ns = sequence [[2..n], [2..n]]
