module Problem.P026 where
import Data.List
import Data.Ord

{-
- Reciprocal cycles
- https://projecteuler.net/problem=26
-}

{-
- [方針1]
- 1/dを0.a1a2a3...と表記する。akは小数点以下の各桁の数字を表す。
-
- akの具体的な値を求めるために、筆算と同じ手順を繰り返す。つまり
-   1. 分母を10倍してdで割った商をp、余りをqとすると、a1=pである。
-   2. qを10倍してdで割った商をp'、余りをq'とすると、a2=p'である。
-   3. q=q'として2の手順を繰り返すと、a3以降も求められる。
-   4. q=0となった場合、割り切れたのでそこで終了する。循環節は
-      存在しない。(長さは0)
-   5. 3を繰り返した結果、一度計算したqが再び出現したら、最初にqが
-      出現した位置からそこまでが循環節である。
-
- [結果]
- 983
- time:2.777817s
-}

recurringCycle :: Integral a => a -> [a]
recurringCycle d = recurringCycle' 1 [] []
  where recurringCycle' q ps qs | q == 0 = []
                                | elem q qs = map fst $ dropWhile ((/= q) . snd) $ reverse $ zip ps qs
                                | otherwise = recurringCycle' q' (p:ps) (q:qs)
          where (p, q') = (q * 10) `divMod` d

solve :: Integral a => a -> Integer
solve n = toInteger $ maximumBy (comparing (length . recurringCycle)) [1..n]
