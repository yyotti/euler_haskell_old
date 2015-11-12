module Problem.P007 where
import Common.Arithmetic

{-
- 10001st prime
- https://projecteuler.net/problem=7
-}

{-
- [方針1]
- 素直に素数列のn番目を取得する。
-
- [結果]
- 104743
- time:0.916264s
-}
solve :: Int -> Integer
solve n = primes !! (n - 1)
