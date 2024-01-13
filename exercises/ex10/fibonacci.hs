-- fib :: (Eq t, Num t, Num a) => t -> a
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n - 1) + fib (n - 2)

fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)