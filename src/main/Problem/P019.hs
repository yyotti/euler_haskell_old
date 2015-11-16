module Problem.P019 where

{-
- Counting Sundays
- https://projecteuler.net/problem=19
-}

{-
- [方針1]
- 1901年1月から2000年12月までの各月の1日について、1900年1月1日からの日数を閏年を
- 考慮しつつ計算し、その日数が7で割り切れる月をカウントする。
-
- [結果]
- 171
- time:0.000204s
-}

isLeapYear :: Int -> Bool
isLeapYear y = mod y 400 == 0 || mod y 100 /=0 && mod y 4 == 0

dateCount :: Int -> Int -> Int
dateCount y 2 | isLeapYear y = 29
              | otherwise = 28
dateCount _ 4 = 30
dateCount _ 6 = 30
dateCount _ 9 = 30
dateCount _ 11 = 30
dateCount _ _ = 31

solve :: Integer
solve = countBy ((== 0) . (`mod` 7)) $ scanl (\ z c -> z + c - dateCount1900) dateCount1900 dateCounts
  where dateCount1900 = sum $ map (dateCount 1900) [1..12]
        dateCounts = concatMap (\ y -> map (\ m -> dateCount y m) [1..12]) [1901..2000]
        countBy _ [] = 0
        countBy f (x:xs) | f x = 1 + countBy f xs
                         | otherwise = countBy f xs
