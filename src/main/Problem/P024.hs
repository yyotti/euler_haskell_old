module Problem.P024 where
import Data.List
import Common.Arithmetic

{-
- Lexicographic permutations
- https://projecteuler.net/problem=24
-}

{-
- [方針1]
- permutationsを使えば簡単と思ったら、Haskellのpermutationsは今回の
- 希望に沿った並び方をしてくれないらしいので、自分で作る。
-
- [結果]
- 2783915460
- time:0.51052s
-}

{-
- [方針2]
- permutationsで全ての順列を列挙するのは時間がかかるので、i番目をピンポイントに
- 生成する方法を考える。
-
- 順列を生成する元のリストを[a0,a1,...,ak]とする。ただしa0からakは辞書順に並んで
- いるものとする。(つまり a0 < a1 < ... < ak)
- このとき、a0を先頭に置く並べ方は k! 通りある。もし
-   k! < i
- であるなら、a0を先頭にした並べ方の中にi番目は無いことが分かる。逆に
-   k! >= i
- であるなら、a0を先頭にした並べ方の中にi番目が存在する。
- 後者である場合、先頭がa0で確定するため、残りの[a1,...,ak]で順列を作り、そのi番目
- を選択すればよいことになる。
- 前者であるなら、a1からakを先頭にした並べ方もa0のときと同じように k! 通りあるわけ
- なので、
-   i / k!  (小数点以下は切り捨て)
- で何番目の要素を先頭に置くかが分かる。その後、残りの要素で順列を作り、その中の
-   i mod k!
- 番目の要素をとればよい。
-
- 前者・後者で分けて考えたが、商と余りを求める操作はk!とiの大小関係によらず同じ結果
- となるので、
-   リストの i/k! 番目を先頭とする
-   残りの要素で順列を作りそれの i mod k! 番目をとる
- の操作を再帰的に繰り返せばよい。
-
- [結果]
- 2783915460
- time:0.000084s
-}

permutation :: Eq a => [a] -> Int -> [a]
permutation [] _ = []
permutation ls 0 = ls
permutation ls i = h : (permutation rs (i `mod` f))
  where f = (fact . length . tail) ls
        h = ls !! (i `div` f)
        rs = delete h ls

solve :: Integral a => [a] -> Int -> Integer
solve ns i = toInteger $ toNum $ permutation ns (i - 1)
  where toNum = foldl' (\ n k -> n * 10 + k) 0
