module Common.Arithmetic where
  fib :: [Integer]
  fib = 1 : 1 : (map (\ (a, b) -> a + b) $ zip fib $ tail fib)
