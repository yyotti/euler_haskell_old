module Problem.P039 where
import Data.Ord
import Data.List
import Common.Arithmetic

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
-
- [コミット]
- b1000d3
-}

{-
- [方針2]
- 要するにピタゴラス数を列挙すればいいわけで、Problem 9とよく似た問題である。
-
- ただし今回は、3辺の和が目的の値となるピタゴラス数を1組見つければいいわけでは
- なく、全ての組を見つけなければならない。
- ======== Problem 9 から引用 ========
- 原始ピタゴラス数を列挙する方法は
-   (x, y, z) = (m^2 - n^2, 2mn, m^2 + n^2)
-   ただし m,n ∈  N, m > n, (m - n) ≡ 1 (mod 2), gcd(m, n) = 1
- であるから、
-   a + b + c = w(m^2 - n^2 + 2mn + m^2 + n^2)
-             = w(2m^2 + 2mn)
-             = 2wm(m + n) = k
- よって、
-   k ≡  0 (mod 2m(m + n))
- となる(m,n)を求め、w = k/(2m(m + n))として(a, b, c)を求めればよい。
-
- なお、kが奇数の場合には条件を満たす(m,n)は存在しない。
- また、
-   2wm(m + n) = k
- より、w = 1、n = 0として
-   2m^2 <= k
- がmの上限である。
- ======== Problem 9 から引用 ========
- ということなので、これに従って 2m(m + n) がkの約数となる(m,n)の組を
- カウントする。ただし、上記の「kが奇数の場合には...」という条件は
- 今回は適用されないので、奇数のkに対しても調べていく。
-
- [結果]
- 840
- time:0.002346s
-}

pythatoreanTripletCount :: Integral a => a -> Int
pythatoreanTripletCount p = length $ filter (\ (m, n) -> p `mod` (2 * m * (m + n)) == 0) $ mns
  where mns = takeWhile (\ x -> 2 * (fst x)^2 <= p) primitivePythagoreanMNs

solve :: Int -> Integer
solve n = toInteger $ fst $ maximumBy (comparing snd) $ map (\ p -> (p, pythatoreanTripletCount p)) [1 .. n]

