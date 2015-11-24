module Problem.P034 where
import Common.Util
import Common.Arithmetic
import Data.List

{-
- Digit factorials
- https://projecteuler.net/problem=34
-}

{-
- [方針1]
- k桁の整数nについて、各桁を ak,a(k-1),...,a1 とする。
- このとき各桁の階乗和Sは
-   S = (ak)! + (a(k-1))! + ... + (a1)!
- となる。
- 各桁の数字は全て0以上9以下なので、Sの最大値Smaxは
-   Smax = k*9! = 362880k
- である。
-
- k = 6のとき Smax = 2177280 (7桁)
- k = 7のとき Smax = 2540160 (7桁)
- k = 8のとき Smax = 2903040 (7桁)
- となるから、nが8桁になると常に n > S となってしまい、一致することは
- ありえない。よって n <= 2540160 の範囲で探せばよいが、問題に 1! = 1 と
- 2! = 2 は除外するとあるので、 3 <= n <= 2540160 の範囲で探す。
-
- [結果]
- 40730
- time:2.190381s
-
- [コミット]
- f17797c
-}

{-
- [方針2]
- 桁ごとの階乗の和は、整数nの各桁の並び順を変更しても変わらないので、
- 各桁の階乗和と一致するnが見つかったら、nの各桁の数字を並べ変えた
- 数は対象から除外してよい。
-
- 逆に考えれば、0から9までの数字の階乗を組み合わせて和をとることで
- 生成できる数字を抽出すればよいことになる。
- この組み合わせを
-   a(k-1)はak以上の数字を使えばよい
- というルールで生成していく。
-
- なお、組み合わせを作る際に、0と1は区別する必要がない。
- 0! = 1、1! = 1であるから、[0,0,0]も[1,1,1]も、階乗和をとればともに
- 3となるため、重複を避けるために組み合わせは0を含めずに作る。
-
- [結果]
- 40730
- time:0.523135s
-}

facts :: [Int]
facts = map fact [0..9]

nums :: Int -> [[Int]]
nums k | k < 1 = []
       | k == 1 = map (:[]) [3..9]
       | otherwise = concatMap (\ i -> map (i:) $ nums' i (k-1)) [1..9]
  where nums' m 1 = map (:[]) [m..9]
        nums' m k = concatMap (\ i -> map (i:) $ nums' i (k-1)) [m..9]

factSum :: [Int] -> Int
factSum = sum . map (facts !!)

isDigitFactorial :: Int -> Bool
isDigitFactorial n = n == factSum (digits n)

solve :: Integer
solve = toInteger $ sum $ filter isDigitFactorial $ dropWhile (<= 2) $ nub $ sort $ map factSum $ concatMap nums [1..7]
