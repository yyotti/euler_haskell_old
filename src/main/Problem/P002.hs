module Problem.P002 where
import Data.Time

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
-}
fib :: Int -> Integer
fib 1 = 1
fib 2 = 2
fib n | n >= 3 = fib (n - 1) + fib (n - 2)
      | otherwise = 0

solve :: Int -> Integer
solve n = sum $ filter even $ takeWhile ((<= n) . fromIntegral) $ map fib [1..]

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 4000000
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
