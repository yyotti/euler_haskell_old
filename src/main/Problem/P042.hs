module Problem.P042 where
import Common.Util
import Data.Char
import Common.Arithmetic

{-
- Coded triangle numbers
- https://projecteuler.net/problem=42
-}

{-
- [方針1]
- 単語の値wが三角数になるということは、 t(n) = n(n+1)/2 = w より
-   n^2 + n - 2w = 0
- が整数解をもてばよい。よって判別式
-   1 - 4*1*(-2w) = 1 + 8w
- が平方数となればよい。
-
- [結果]
- 162
- time:0.003053s
-}

isTriangleNumber :: Integral a => a -> Bool
isTriangleNumber n = isSquare $ 1 + 8 * n

wordValue :: String -> Int
wordValue = sum . map ((1 +) . flip (-) (ord 'A') . ord)

countTriangleWords :: [String] -> Int
countTriangleWords = length . filter isTriangleNumber . map wordValue

readWords :: String -> IO [String]
readWords = (>>= return . map (tail . init) . splitByComma . head . lines) . readFile

solve :: String -> IO Integer
solve = (>>= return . toInteger . countTriangleWords) . readWords
