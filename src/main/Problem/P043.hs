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
-
- [コミット]
- 4020d1c
-}

{-
- [方針3]
- 方針2の考え方をもとに、「条件を満たすパンデジタル数を探す」のではなく「生成する」
- 方向で考える。
-
-   1. 下3桁の候補を出す
-   2. 1で挙げた数字の1つを(d(8)d(9)d(10))(3桁)とする。
-   3. [0..9]のうちd(8)～d(10)とかぶらないものを1つ選び、d(7)とする。
-   4. d(7)d(8)d(9)が13で割れるものを残す。
-   5. これを繰り返す。
- の手順で、条件を満たすパンデジタル数が生成できることになる。
- 最後に1桁だけ余るのが例外になってしまうので、1も素数としてd(1)d(2)d(3)が1で割り切れる
- かどうかのチェックを加えることで全ての桁を同じ処理で連結していける。
-
- [結果]
- 16695334890
- time:0.000686s
-}

perms :: (Eq a) => Int -> [a] -> [[a]]
perms 0 _ = [[]]
perms _ [] = []
perms n xs = concatMap (\ x -> map (x:) $ perms (n-1) $ filter (/= x) xs) xs

mkNum :: Integral a => [a] -> [a] -> [a] -> [[a]]
mkNum [] ls _ = [ls]
-- mkNum (p:ps) ls ds = concatMap (\ xs -> mkNum ps (head xs : ls) (filter (/= head xs) ds)) concatables
-- ↓後になって読めるかな・・・
mkNum (p:ps) ls ds = concatMap (ap (mkNum ps . (:ls) . head) (flip filter ds . ((/=) . head))) concatNums
  where concatNums = map (: take 2 ls) $ filter ((== 0) . (`mod` p) . toNum . (: take 2 ls)) ds

solve :: Integer
solve = (toInteger . sum . map toNum . concatMap (ap (mkNum primes') ([0..9] \\))) mod17
  where primes' = (reverse . (1:) . takeWhile (< 17)) primes
        mod17 = (filter ((== 0) . (`mod` 17) . toNum) . perms 3) [0..9]

