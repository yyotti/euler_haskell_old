module Problem.P025 where
import Common.Arithmetic
import Data.List
import Data.Maybe

{-
- 1000-digit Fibonacci number
- https://projecteuler.net/problem=25
-}

{-
- [方針1]
- フィボナッチ数列を作って普通にやる
-
- [結果]
- 4782
- time:0.001273s
-}
solve :: Integral a => a -> Integer
solve n = toInteger $ fromJust $ findIndex (\ f -> f >= 10^(n - 1)) fib
