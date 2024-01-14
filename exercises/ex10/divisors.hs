divisors :: Integral a => a -> Int
divisors n = length [x | x <- [n, n - 1..1], rem n x == 0]
