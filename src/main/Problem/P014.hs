module Problem.P014 where
import Data.List
import Data.Ord

{-
- Longest Collatz sequence
- https://projecteuler.net/problem=14
-}

{-
- [方針1]
- 問題に書かれている定義に従って処理する。
-
- [結果]
- 837799
- time:28.338644s
-}

chainLength :: Integral a => a -> Int
chainLength 1 = 1
chainLength n | even n = 1 + (chainLength $ n `div` 2)
              | otherwise = 1 + (chainLength $ 3 * n + 1)

solve :: Integral a => a -> Integer
solve n = toInteger $ fst $ maximumBy (comparing snd) $ map (\ i -> (i, chainLength i)) [1..(n - 1)]
