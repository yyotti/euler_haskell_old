module Problem.P011 where

{-
- Largest product in a grid
- https://projecteuler.net/problem=11
-}

{-
- [方針1]
- 何も考えない。全パターンを調べるだけ。
-
- [結果]
- 70600674
- time:0.009746s
-}

horizontalPart :: Int -> Int -> Int -> [[a]] -> [a]
horizontalPart i j n g | i >= length g = []
                       | j > length g - n = []
                       | otherwise = take n $ drop j $ g !! i

verticalPart :: Int -> Int -> Int -> [[a]] -> [a]
verticalPart i j n g | i > length g - n = []
                     | j >= length g = []
                     | otherwise = map (!! j) $ take n $ drop i g

diagonalPart1 :: Int -> Int -> Int -> [[a]] -> [a]
diagonalPart1 i j n g | i > length g - n = []
                      | i >= length g = []
                      | j > length g - n = []
                      | j >= length g = []
                      | otherwise = map (\ k -> g !! (i + k) !! (j + k)) [0..(n - 1)]

diagonalPart2 :: Int -> Int -> Int -> [[a]] -> [a]
diagonalPart2 i j n g | i > length g - n = []
                      | i >= length g = []
                      | j < n - 1 = []
                      | j >= length g = []
                      | otherwise = map (\ k -> g !! (i + k) !! (j - k)) [0..(n - 1)]

solve :: Int -> [[Int]] -> Integer
solve n g = toInteger $ maximum $ map toProducts points
  where points = concatMap (\ i -> map (\ j -> (i, j)) [0..(length (g !! i))]) [0..(length g - 1)]
        toProducts (i, j) = h `max` v `max` d1 `max` d2
          where h = product $ horizontalPart i j n g
                v = product $ verticalPart i j n g
                d1 = product $ diagonalPart1 i j n g
                d2 = product $ diagonalPart2 i j n g
