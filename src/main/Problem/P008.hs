module Problem.P008 where

{-
- Largest product in a series
- https://projecteuler.net/problem=8
-}

{-
- [方針1]
- 先頭から地道に1つずつやっていく
-
- [結果]
- 23514624000
- time:0.009924s
-
- [コミット]
- d02ca98
-}

{-
- [方針2]
- 先頭から調べるが、0を含む区間があったらそこをスキップする処理を加える。
-
- [結果]
- 23514624000
- time:0.006807s
-
- [コミット]
- cb33430
-}

{-
- [方針3]
- 対象の数字nを桁ごとに分けたリストを0で区切る。
- 区切られた区間ごとに最大値を算出し、その後全体の最大値をとる。
- 区間内に指定された桁数だけの数字がない区間は最大値0としてしまえばよい。
-
- [結果]
- 23514624000
- time:0.004882s
-}
digits :: Integer -> [Integer]
digits n | n < 0 = []
         | otherwise = digits' n []
  where digits' k ls | k < 10 = k : ls
                     | otherwise = digits' (k `div` 10) $ (k `mod` 10) : ls

findLargestProduct :: Int -> [Integer] -> Integer
findLargestProduct _ [] = 0
findLargestProduct k ns = max (findLP nums) (findLargestProduct k rest)
  where (nums, rest) = case span (/= 0) ns of
                            (xs, []) -> (xs, [])
                            (xs, (_:ys)) -> (xs, ys)
        findLP xs | k > length xs = 0
                  | otherwise = max (product $ take k xs) (findLP $ tail xs)

solve :: Int -> Integer -> Integer
solve k n = findLargestProduct k $ digits n
