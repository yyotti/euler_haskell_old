module Problem.P004 where
import Data.Time

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
- time:1.570827s
-}

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations r (x:xs) = comb1 ++ comb2
  where comb1 = map (x:) $ combinations (r - 1) xs
        comb2 = combinations r xs

products :: Integral a => a -> [a]
products n | n < 1 = []
           | otherwise = (map (\ [x, y] -> x * y) $ combinations 2 nums) ++ (map (^(2 :: Int)) nums)
  where nums = [10^(n - 1) .. 10^n - 1]

isPalindrome :: (Show a) => a -> Bool
isPalindrome x = show x == reverse (show x)

solve :: (Integral a, Show a) => a -> a
solve n = maximum $ filter isPalindrome $ products n

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve (3 :: Int)
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
