module Problem.P004 where

{-
- Largest palindrome product
- https://projecteuler.net/problem=4
-}

{-
- [方針1]
- n桁の数の積で表される回文数を求めるので、素直に「n桁 x n桁」を計算する。
- n桁の整数のリストから2つを選択して積を求め、それが回文数になっているかを
- 判定する。
-
- ただし、重複を避けるために組み合わせで求めなければならないが、Haskellには
- permutationsはあってもなぜかcombinationsは無いようなので、自作する。
-
- [結果]
- 906609
- time:0.088629s
-
- [コミット]
- 8b95054
-}

{-
- [方針2]
- 回文数になるためには、掛け合わせる2つの数のいずれか一方が11の倍数でなければ
- ならない。そこで、n桁のリストを2つ作り、片方は11の倍数のみをもつリストにして、
- 2つのリスト間で要素の組み合わせを作る。
-
- [結果]
- 906609
- time:0.01853s
-}

products :: Integral a => a -> [a]
products n | n < 1 = []
           | otherwise = (map product $ sequence [nums1, nums2])
  where nums1 = [10^(n - 1) .. 10^n - 1]
        nums2 | n == 1 = nums1
              | otherwise = filter ((== 0) . (`mod` 11)) nums1

isPalindrome :: (Show a) => a -> Bool
isPalindrome x = show x == reverse (show x)

solve :: (Integral a, Show a) => a -> Integer
solve n = toInteger $ maximum $ filter isPalindrome $ products n
