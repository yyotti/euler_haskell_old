import System.Environment
import Data.Time
import Text.Printf

import qualified Problem.P001 as P001
import qualified Problem.P002 as P002
import qualified Problem.P003 as P003
import qualified Problem.P004 as P004
import qualified Problem.P005 as P005
import qualified Problem.P006 as P006
import qualified Problem.P007 as P007
import qualified Problem.P008 as P008
import qualified Problem.P009 as P009
import qualified Problem.P010 as P010
import qualified Problem.P011 as P011
import qualified Problem.P012 as P012
import qualified Problem.P013 as P013
import qualified Problem.P014 as P014
import qualified Problem.P015 as P015
import qualified Problem.P016 as P016
import qualified Problem.P018 as P018
import qualified Problem.P019 as P019
import qualified Problem.P020 as P020
import qualified Problem.P021 as P021
import qualified Problem.P022 as P022
import qualified Problem.P023 as P023
import qualified Problem.P024 as P024
import qualified Problem.P025 as P025
import qualified Problem.P026 as P026
import qualified Problem.P027 as P027
import qualified Problem.P028 as P028
import qualified Problem.P029 as P029

p008Num :: Integer
p008Num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

p011grid :: [[Int]]
p011grid =
  [
    [ 8,  2, 22, 97, 38, 15,  0, 40,  0, 75,  4,  5,  7, 78, 52, 12, 50, 77, 91,  8],
    [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48,  4, 56, 62,  0],
    [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30,  3, 49, 13, 36, 65],
    [52, 70, 95, 23,  4, 60, 11, 42, 69, 24, 68, 56,  1, 32, 56, 71, 37,  2, 36, 91],
    [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
    [24, 47, 32, 60, 99,  3, 45,  2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
    [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
    [67, 26, 20, 68,  2, 62, 12, 20, 95, 63, 94, 39, 63,  8, 40, 91, 66, 49, 94, 21],
    [24, 55, 58,  5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
    [21, 36, 23,  9, 75,  0, 76, 44, 20, 45, 35, 14,  0, 61, 33, 97, 34, 31, 33, 95],
    [78, 17, 53, 28, 22, 75, 31, 67, 15, 94,  3, 80,  4, 62, 16, 14,  9, 53, 56, 92],
    [16, 39,  5, 42, 96, 35, 31, 47, 55, 58, 88, 24,  0, 17, 54, 24, 36, 29, 85, 57],
    [86, 56,  0, 48, 35, 71, 89,  7,  5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
    [19, 80, 81, 68,  5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77,  4, 89, 55, 40],
    [ 4, 52,  8, 83, 97, 35, 99, 16,  7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
    [88, 36, 68, 87, 57, 62, 20, 72,  3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
    [ 4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18,  8, 46, 29, 32, 40, 62, 76, 36],
    [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74,  4, 36, 16],
    [20, 73, 35, 29, 78, 31, 90,  1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57,  5, 54],
    [ 1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52,  1, 89, 19, 67, 48]
    ]

p018triangle :: [[Int]]
p018triangle =
  [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20,  4, 82, 47, 65],
    [19,  1, 23, 75,  3, 34],
    [88,  2, 77, 73,  7, 63, 67],
    [99, 65,  4, 28,  6, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [ 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23]
    ]

problems :: [IO Integer]
problems =
  [
    return 0, -- dummy for sliding index
    return $ P001.solve 1000,
    return $ P002.solve 4000000,
    return $ P003.solve 600851475143,
    return $ P004.solve (3 :: Int),
    return $ P005.solve 1 20,
    return $ P006.solve 100,
    return $ P007.solve 10001,
    return $ P008.solve 13 p008Num,
    return $ P009.solve 1000,
    return $ P010.solve 2000000,
    return $ P011.solve 4 p011grid,
    return $ P012.solve 500,
    return $ P013.solve,
    return $ P014.solve 1000000,
    return $ P015.solve 20,
    return $ P016.solve 2 1000,
    return $ 0, -- P017 is pending
    return $ P018.solve p018triangle,
    return $ P019.solve,
    return $ P020.solve 100,
    return $ P021.solve 10000,
    P022.solve "src/main/resources/p022_names.txt",
    return $ P023.solve 28123,
    return $ P024.solve [0..9] 1000000,
    return $ P025.solve 1000,
    return $ P026.solve 1000,
    return $ P027.solve 1000,
    return $ P028.solve 1001,
    return $ P029.solve 100
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
       [] -> printAll
       (n:_) -> let i = read n :: Int in printOne i

time :: Int -> IO()
time i = do
  _ <- printf "P%03d\n" i
  x <- getCurrentTime
  print =<< problems !! i
  y <- getCurrentTime

  putStr "time:"
  print $ diffUTCTime y x

printOne :: Int -> IO ()
printOne n | n < 1 || n >= length problems = putStrLn $ "problem number out of range :" ++ show n
           | otherwise = time n

printAll :: IO ()
printAll = do
  let withIdx = zip [(0 :: Int)..] problems
  mapM_ (\ (i, a) -> do ans <- a;  printf "P%03d: %d\n" i ans) $ tail withIdx
