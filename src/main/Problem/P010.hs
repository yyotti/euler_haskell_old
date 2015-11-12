module Problem.P010 where

{-
- Summation of primes
- https://projecteuler.net/problem=10
-}

{-
- [方針1]
- 素数列の小さい方から指定された最大値まで取り出して和をとる
-
- [結果]
- 142913828922
- time:36.233071s
-
- [コミット]
- ce3ae63
-}

{-
- [方針2]
- 最大値が決まっているので、エラトステネスの篩で素数を取り出して
- 和をとる。
-
- [結果]
- 142913828922
- time:15.869627s
-}
sieve :: Int -> [Int]
sieve n | n < 2 = []
        | otherwise = 2 : sieve' [3,5..n]
  where sieve' [] = []
        sieve' ls@(x:_) | x * x > n = ls
        sieve' (x:xs) = let rest = filter (\ k -> k `mod` x /= 0) xs in x : (sieve' rest)

solve :: Int -> Integer
solve = sum . map toInteger . sieve
