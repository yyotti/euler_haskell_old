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
-
- [コミット]
- 0cb804d
-}

{-
- [方針2]
- 三角形の行数をnとすれば2^(n-1)通りを調べる必要があるので、工夫する。
-
- 三角形の中のある数字に注目すると、そこより下のルートで和が最大のルートは、
-   「左に行くルート」の中で和が最大となるルート
-   「右に行くルート」の中で和が最大となるルート
- をそれぞれ算出し、さらに2つのうち大きい方を採用すればよい。
- 三角形の底辺とその上の行でこのルールを適用すれば、
-   底辺の数字で隣り合っているものどうしを比較し、大きい方を残す
- ということになる。問題の小さい三角形では、底辺は[8,5,9,3]であるから、
-   ・8と5で比較して大きい方を残す・・・8を残す
-   ・5と9で比較して大きい方を残す・・・9を残す
-   ・9と3で比較して大きい方を残す・・・9を残す
- となり、残るのは[8,9,9]である。
- 結果、三角形は
-   [3],
-   [7,4],
-   [2,4,6],
-   [8,9,9]
- となる。
- 底辺に残ったそれぞれの要素は1つ上の行の要素に対応するので、それぞれ和を
- とり
-   [3],
-   [7,4],
-   [10,13,15]
- とできる。これを要素が1つになるまで繰り返せば、残っているのはルートの数字の
- 和の最大値である。
-
- [結果]
- 1074
- time:0.000069s
-}

pairs :: [a] -> [[a]]
pairs [] = []
pairs [_] = []
pairs (x:y:zs) = [x, y] : pairs (y:zs)

findMax :: Integral a => [[a]] -> a
findMax [[x]] = x
findMax (xs:ys:zs) = findMax $ zipWith (+) xs' ys : zs
  where xs' = map maximum $ pairs xs
findMax _ = 0

solve :: Integral a => [[a]] -> Integer
solve = toInteger . findMax . reverse
