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
-}

triangles :: Integral a => [a]
triangles = 1 : zipWith (+) triangles [2..]

pentagonals :: Integral a => [a]
pentagonals = 1 : zipWith (+) pentagonals [4,7..]

hexagonals :: Integral a => [a]
hexagonals = 1 : zipWith (+) hexagonals [5,9..]

findNextSame :: Integral a => [a] -> [a] -> [a] -> a
findNextSame (h:hs) ps ts | head (dropWhile (< h) ps) == h && head (dropWhile (< h) ts) == h = h
                          | otherwise = findNextSame hs (dropWhile (< h) ps) (dropWhile (< h) ts)
findNextSame _ _ _ = 0 -- ここには入らないはず

solve :: Int -> Integer
solve n = toInteger $ findNextSame (dropWhile (<= n) hexagonals) (dropWhile (<= n) pentagonals) (dropWhile (<= n) triangles)
