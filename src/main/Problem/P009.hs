module Problem.P009 where
import Data.Time

{-
- Special Pythagorean triplet
- https://projecteuler.net/problem=9
-}

{-
- [方針1]
- cが斜辺、a,bが直角を挟む2つの辺なので、大小関係として
-   a <= b < c
- としてよい。
- また
-   a + b + c = n
- とすると、
-   c = n - a - b
- であるから、
-   b < n - a - b
- より
-   b < (n - a)/2
- であり、
-   a <= b < (n - a)/2
- より
-   a <= n/3
- である。
-
- あとは上記の関係を満たす(a, b, c)について、ピタゴラス数に
- なるものを探し積を求めればよい。
-
- [結果]
- 31875000
- time:0.113426s
-
- [コミット]
- 21a086d
-}

{-
- [方針2]
- (x, y, z)を原始ピタゴラス数とする。
- wを自然数とすれば、(a, b, c)は
-   (a, b, c) = (wx, wy, wz)
- と表せる。
- よって原始ピタゴラス数を列挙し、
-   a + b + c = w(x + y + z) = k  (kは与えられた a + b + c の値)
- となる(w, x, y, z)を抽出すればよい。
-
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
-
- [結果]
- 31875000
- time:0.000201s
-}

primitivePythagoreanMNs :: [(Int, Int)]
primitivePythagoreanMNs = concatMap (\ m -> map (\ n -> (m, n)) $ filter (\ n -> odd (m - n) && gcd m n == 1) [1..(m - 1)]) [1..]

findSpecialPythagoreanTriprets :: Int -> [(Int, Int, Int)]
findSpecialPythagoreanTriprets k | odd k = []
                                 | otherwise = map toABC $ filter (\ (m, n) -> k `mod` (2 * m * (m + n)) == 0) mns
  where mns = takeWhile (\ (m, _) -> 2 * m * m <= k) primitivePythagoreanMNs
        toABC (m, n) = let w = k `div` (2 * m * (m + n))
                           in (w * (m * m - n * n), w * 2 * m * n, w * (m * m + n * n))

solve :: Int -> Int
solve n = case findSpecialPythagoreanTriprets n of
               [] -> 0
               ((a, b, c):_) -> a * b * c

main :: IO ()
main = do
  x <- getCurrentTime
  print $ solve 1000
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x
