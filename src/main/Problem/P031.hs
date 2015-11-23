module Problem.P031 where

{-
- Coin sums
- https://projecteuler.net/problem=31
-}

{-
- [方針1]
- ヒューリスティックな手法でやる。
-
- 作る金額をn[p]とする。
- まず、
-   1[p], 2[p], 5[p], 10[p], 20[p], 50[p], 100[p], 200[p]
- の中から、n[p]以下の硬貨を抽出して降順に並べる。そのリスト
- をCとする。
-
- Cの先頭(最も金額の大きい硬貨)の硬貨を見る。それをcとする。
- nを作るのにcを最大何枚使えるかを計算する。
- つまり
-   n = c*m + r
- となる最大のmを算出する。
- その後、
-   ・cをm枚とc未満の硬貨を使ってnを作る方法
-   ・cを(m-1)枚とc未満の硬貨を使ってnを作る方法
-   ・  ...
-   ・cを使わずにc未満の硬貨を使ってnを作る方法
- をそれぞれ求めて足せばよい。
-
- cをm'枚とc未満の硬貨を使ってnを作る方法は、
-   n' = n - c*m'
- とすれば、「c未満の硬貨を使ってn'を作る方法」となるため
- 同じ方法で計算できる。
-
- [結果]
- 73682
- time:0.002835s
-}

coins :: Integral a => [a]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

countPatterns :: Integral a => a -> [a] -> a
countPatterns _ [] = 0
countPatterns 0 _ = 1
countPatterns n [c] | n `mod` c == 0 = 1
                    | otherwise = 0
countPatterns n (c:cs) = sum $ map (\ i -> countPatterns (n - c * i) cs) [0..m]
  where m = n `div` c

solve :: Int -> Integer
solve n = toInteger $ countPatterns n cs
  where cs = reverse $ takeWhile (<= n) coins
