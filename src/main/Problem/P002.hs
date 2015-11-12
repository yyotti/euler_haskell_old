module Problem.P002 where
import Common.Arithmetic

{-
- Even Fibonacci numbers
- https://projecteuler.net/problem=2
-}

{-
- [方針1]
- フィボナッチ数列の定義に従って第n項を再帰的に求める関数を作り、400万以下の
- 偶数値の項の和をとる。
- 便宜上、n < 1の場合は0とする。(エラー処理がめんどくさいため)
-
- [結果]
- 4613732
- time:13.474108s
-
- [コミット]
- 45db11e
-}

{-
- [方針2]
- フィボナッチ数列の無限リストを定義し、値が400万以下の要素をさらに偶数で絞って
- 和をとる。
-
- [結果]
- 4613732
- time:0.00021s
-}
fib' :: [Integer]
fib' = tail fib

solve :: Int -> Integer
solve n = sum $ filter even $ takeWhile ((<= n) . fromIntegral) fib'
