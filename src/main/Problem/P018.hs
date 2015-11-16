module Problem.P018 where

{-
- Maximum path sum I
- https://projecteuler.net/problem=18
-}

{-
- [方針1]
- 問題の注釈を信じるなら、15段で全パターンを調べても16384通りしかないらしいので、
- まずはストレートに、全てのルートを検出して解いてみる。
-
- 全てのルートを検出するためには、頂点から辿って
-   左に行くルート・・・問題の小さい三角形なら 3 -> 7 と行くルート
-   右に行くルート・・・問題の小さい三角形なら 3 -> 4 と行くルート
- をそれぞれ再帰的に求め、それぞれのルートに対して頂点の3を先頭に加えてやればよい。
-
- [結果]
- 1074
- time:0.022841s
-}

findRoutes :: Integral a => [[a]] -> [[a]]
findRoutes [] = []
findRoutes [xs] = [xs]
findRoutes (xs:ys) = map (top:) $ findRoutes left ++ findRoutes right
  where top = head xs
        left = map init ys
        right = map tail ys

solve :: Integral a => [[a]] -> Integer
solve ls = toInteger $ maximum $ map sum $ findRoutes ls
