module Problem.P036 where
import Common.Util

{-
- Double-base palindromes
- https://projecteuler.net/problem=36
-}

{-
- [方針1]
- 0から順に2進数表記に変換し、回文数であるか判定する。
-
- [結果]
- 872187
- time:0.099149s
-}

bin :: Integral a => a -> String
bin 0 = "0"
bin 1 = "1"
bin n = bin m ++ bin r
  where (m, r) = n `divMod` 2

solve :: Int -> Integer
solve n = toInteger $ sum $ filter (\ i -> isPalindrome (show i) && isPalindrome (bin i)) [0..(n-1)]
