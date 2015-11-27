module Problem.P040 where
import Common.Util

{-
- Champernowne's constant
- https://projecteuler.net/problem=40
-}

{-
- [方針1]
- チャンパーノウン定数の小数点以下を配列として生成して、指定され
- た項の積をとる。インデックスを合わせるため、初項は便宜上0とする。
-
- [結果]
- 210
- time:0.317461s
-}

champernownes :: [Int]
champernownes = concatMap digits [0..]

solve :: [Int] -> Integer
solve = toInteger . product . map (champernownes !!)
