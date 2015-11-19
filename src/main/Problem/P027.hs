module Problem.P027 where
import Common.Arithmetic
import Data.List
import Data.Ord

{-
- Quadratic primes
- https://projecteuler.net/problem=27
-}

{-
- [方針1]
- ストレートに、指定された値の範囲内で探す。
-
- [結果]
- 中断(数分かかっても終了しない)
-}

{-
- [方針2]
-   f(n) = n^2 + an + b
- とする。
-
- n = 0 のときに素数でなければならないので、bは素数でなければならない。
- また、
-   f(n) = n(n + a) + b
- であるから、n = bもしくは(n + a) = bになったら必ずf(n)は素数でなく
- なる。
-   a >= 0 のとき n <= n + a -> (n + a) の方が早くbになる
-   a < 0  のとき n > n + a  -> n の方が早くbになる
- ということであるから、aは負である方がいい。
-
- [結果]
- -59231
- time:0.445627s
-}

primesCount :: Integral a => a -> a -> Int
primesCount a b = length $ takeWhile isPrime nums
  where nums = map (\ n -> n^2 + a * n + b) [0..]

solve :: Int -> Integer
solve m = toInteger $ product $ maximumBy (comparing pc) ab
  where pc [a, b] = primesCount a b
        pc _ = 0
        as = [(-m + 1)..(-1)]
        bs = takeWhile (< m) primes
        ab = sequence [as, bs]
