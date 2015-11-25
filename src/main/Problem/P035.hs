module Problem.P035 where
import Common.Arithmetic
import Common.Util
import Control.Monad

{-
- Circular primes
- https://projecteuler.net/problem=35
-}

{-
- [方針1]
- 100万未満の素数を列挙して、巡回素数かどうか順に調べていく。
-
- [結果]
- 55
- time:0.755195s
-}

{-
- [方針2]
- nが巡回素数であるためには、下記の条件を満たさなければならない。
-   1. nは素数である
-   2. 桁を回転させたときに得られる数が素数でなければならない。
- 2の条件を満たすためには、nの全ての桁の数字が5以外の奇数でなければならない。
- 偶数や5を含むと、桁を回転させていけばいつかそれが1の位になり、明らかに
- 素数ではなくなる。
-
- ただし、1桁の素数は巡回素数であるから、2と5はカウントしてよい。
-
- [結果]
- 55
- time:0.409705s
-}

cycles :: Integral a => a -> [a]
cycles n = cycles' n e
  where e = length $ digits n
        cycles' _ 0 = []
        cycles' n k = n : cycles' (m * 10 + d) (k-1)
          where (d, m) = n `divMod` (10^(e-1))

allDigitsOdd :: Integral a => a -> Bool
allDigitsOdd 0 = True
allDigitsOdd n = allDigitsOdd m && odd r && r /= 5
  where (m, r) = n `divMod` 10

solve :: Int -> Integer
solve n = toInteger $ length lowers + length highers
  where (lowers, primes') = span (< 10 `min` n) primes
        highers = filter (ap ((&&) . allDigitsOdd) (all isPrime . cycles)) $ takeWhile (<= n) primes'
