module Problem.P045 where

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
-}

triangles :: Integral a => [a]
triangles = 1 : zipWith (+) triangles [2..]

pentagonals :: Integral a => [a]
pentagonals = 1 : zipWith (+) pentagonals [4,7..]

hexagonals :: Integral a => [a]
hexagonals = 1 : zipWith (+) hexagonals [5,9..]

findNextSame :: Integral a => [a] -> [a] -> [a] -> a
findNextSame (t:ts) ps hs | head (dropWhile (< t) ps) == t && head (dropWhile (< t) hs) == t = t
                          | otherwise = findNextSame ts (dropWhile (< t) ps) (dropWhile (< t) hs)
findNextSame _ _ _ = 0 -- ここには入らないはず

solve :: Int -> Integer
solve n = toInteger $ findNextSame (dropWhile (<= n) triangles) (dropWhile (<= n) pentagonals) (dropWhile (<= n) hexagonals)
