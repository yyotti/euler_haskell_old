module Problem.P020 where
import Common.Arithmetic
import Common.Util

{-
- Factorial digit sum
- https://projecteuler.net/problem=20
-}

{-
- [方針1]
- 素直にやるしかない気がするが
-
- [結果]
- 648
- time:0.000205s
-}
solve :: Integral a => a -> Integer
solve = toInteger . sum . digits . fact
