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

{-
- [方針2]
- 2進数表記したときに回文数になるためには奇数でなければならない。
- 0は偶数で条件を満たすが、和をとった場合には無くとも問題ないので、
- 1から始まる奇数についてのみ判定していけばよい。
-
- [結果]
- 872187
- time:0.062355s
-}

bin :: Integral a => a -> String
bin 0 = "0"
bin 1 = "1"
bin n = bin m ++ bin r
  where (m, r) = n `divMod` 2

solve :: Int -> Integer
solve n = toInteger $ sum $ filter (\ i -> isPalindrome (show i) && isPalindrome (bin i)) [1,3..(n-1)]
