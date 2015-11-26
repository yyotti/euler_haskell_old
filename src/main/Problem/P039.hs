module Problem.P039 where
import Data.Ord
import Data.List

{-
- Integer right triangles
- https://projecteuler.net/problem=39
-}

{-
- [方針1]
- (a, b, c)はピタゴラス数になるので、
-   a^2 + b^2 = c^2
- を満たす。また、a = bの場合は
-   2a^2 = c^2
- となり、cが整数にならないため、a /= b である。
-
- a < b < c とする。
-   a + b + c = p
- であることから、下記の関係が成立する。
-   a <= p / 3 (小数点以下は切り捨て)
-   p / 3 <= b <= 2p / 3  (小数点以下は切り捨て)
-   c = p - a - b
- これを元にして (a, b, c) の組を作り、ピタゴラス数となっている
- 組の個数が最大となるpを探せばよい。
-
- [結果]
- 840
- time:1.360704s
-}

isPythagoreanTriples :: Integral a => a -> a -> a -> Bool
isPythagoreanTriples a b c = a^2 + b^2 == c^2

pythatoreanTripletCount :: Integral a => a -> Int
pythatoreanTripletCount p = length $ filter (\ (a, b, c) -> isPythagoreanTriples a b c) $ triplet p
  where triplet p = concatMap (\ a -> map (\ b -> (a, b, p - a - b)) [aMax p .. bMax p]) [1 .. aMax p]
        aMax = flip div 3
        bMax = flip div 3 . (*) 2

solve :: Int -> Integer
solve n = toInteger $ fst $ maximumBy (comparing snd) $ map (\ p -> (p, pythatoreanTripletCount p)) [1 .. n]

