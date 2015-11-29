module Problem.P043 where
import Common.Arithmetic
import Common.Util
import Data.List
import Control.Monad

{-
- Sub-string divisibility
- https://projecteuler.net/problem=43
-}

{-
- [方針1]
- 0から9のパンデジタル数を1つ1つ調べていく。
-
- [結果]
- 16695334890
- time:2.219413s
-
- [コミット]
- 68e9d23
-}

{-
- [方針2]
- 下3桁が17の倍数になるので、下3桁の候補をまず列挙する。
- 候補は、先頭0を許しつつ各桁の数字が重複しないものを挙げる。
- 候補で挙がった3つの数字を除いた7個の数字を順列で並べ、条件を満たすか
- を判定していけばよい。
-
- [結果]
- 16695334890
- time:0.21519s
-}

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations r (x:xs) = comb1 ++ comb2
  where comb1 = map (x:) $ combinations (r - 1) xs
        comb2 = combinations r xs

perms :: (Eq a) => Int -> [a] -> [[a]]
perms = (concatMap permutations .) . combinations

mod17 :: Integral a => [[a]]
mod17 = (filter ((== 0) . (`mod` 17) . toNum) . perms 3) [0..9]

sliding :: Int -> [a] -> [[a]]
sliding _ [] = []
sliding n ls@(_:xs) | length ls <= n = [ls]
                    | otherwise = take n ls : sliding n xs

slidingNums :: Integral a => Int -> [a] -> [a]
slidingNums = (map toNum .) . sliding

isDivisiblePandigital :: Integral a => [a] -> Bool
isDivisiblePandigital = all (== 0) . zipWith (flip mod) primes . (tail . slidingNums 3)

pandigitals :: Integral a => [[a]]
pandigitals = concatMap (ap (map . flip (++)) (permutations . ([0..9] \\))) mod17

solve :: Integer
solve = (toInteger . sum . map toNum) divisiblePandigitals
  where divisiblePandigitals = filter (ap ((&&) . ((/= 0) . head)) isDivisiblePandigital) pandigitals
