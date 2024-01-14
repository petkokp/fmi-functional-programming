count :: Eq a => [a] -> a -> Int
count lst x = 1 + length [y | y <- lst, y == x]

clean :: Eq a => [a] -> a -> [a]
clean lst x = filter (x /=) lst

histogram :: Eq a => [a] -> [(a, Int)]
histogram [] = []
histogram (x:xs) = (x, count xs x) : histogram (clean xs x)