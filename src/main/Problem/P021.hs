module Problem.P021 where
import Data.List as List
import Data.Map as Map

{-
- Amicable numbers
- https://projecteuler.net/problem=21
-}

{-
- [方針1]
- 指定された数未満のd(n)を求め、それをmとする。
-   d(m) = n  (m ≠ n)
- である場合をカウントしていく。
- ただし重複が考えられるので、最後に2で割らなければならない。
- また、場合によっては d(n) が最大値を超える可能性があるので注意する。
-
- nが
-   n = p1^e1 * p2^e2 * ... * pk^ek  (pk は素数、ekは自然数)
- と素因数分解できるとき、nの約数の和は
-   (1 + p1 + p1^2 + ... + p1^e1)*(1 + p2 + p2^2 + ... + p2^e2)
- で表すことができる。
-
- [結果]
- 31626
- time:0.418841s
-
- [コミット]
- 3f57693
-}

{-
- [方針2]
- 素因数分解ではなくエラトステネスの篩の要領で約数の和を求める。
- 1からnまでのリストを作り、それぞれの要素を(i,0)としておく。
- 先頭から順に要素を辿り、自分自身の倍数に対して自分の値を加算していく。
- つまり
-   1・・・2以上の全ての要素に1を加える
-   2・・・3以上の2の倍数に2を加える
-   3・・・4以上の3の倍数に3を加える
- と繰り返していく。(自分自身を含めてしまうと友愛数ができない)
-
- あとはリストの中から友愛数となるペアを抽出して和をとればよい。
-
- [結果]
- 31626
- time:0.099711s
-}

sieveLike :: Integral a => a -> Map a a
sieveLike n = List.foldl' add m [1..n]
  where m = fromList $ List.map (\ i -> (i, 0)) [1..n]
        add m i = List.foldl' (\ m k -> insertWith (+) (i * k) i m) m [2..n `div` i]

filterAmicableNumbers :: Integral a => a -> Map a a -> [a]
filterAmicableNumbers n m = foldlWithKey' addAmicableNumber [] m
  where addAmicableNumber ls i s | i < s && s <= n && findWithDefault 0 s m == i = i : s : ls
                                 | otherwise = ls

solve :: (Integral a, Show a) => a -> Integer
solve n = toInteger $ sum $ filterAmicableNumbers m $ sieveLike m
  where m = n - 1
