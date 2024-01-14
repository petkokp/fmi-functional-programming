count :: Eq a => [a] -> a -> Int
count lst x = 1 + length [y | y <- lst, y == x]

clean :: Eq a => [a] -> a -> [a]
clean lst x = filter (x /=) lst

histogram :: Eq a => [a] -> [(a, Int)]
histogram [] = []
histogram (x:xs) = (x, count xs x) : histogram (clean xs x)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort lst@(x:xs) = quickSort [ y | y<-xs, y < x]
                    ++           [ y | y<-lst, y == x]
                    ++ quickSort [ y | y<-xs, y > x]

quickSortBy :: (a -> a -> Bool) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy _ [x] = [x]
quickSortBy cmp lst@(x:xs) = quickSortBy cmp [ y | y<-xs, y `cmp` x]
                          ++                 [ y | y<-lst, not (x `cmp` y || y `cmp` x)]
                          ++ quickSortBy cmp [ y | y<-xs, x `cmp` y]

quickSortOn :: Ord b => (a -> b) ->  [a] -> [a]
quickSortOn f lst = map fst $ quickSortBy (\p1 p2 -> snd p1 < snd p2) [ (x, f x) | x<-lst ]

specialSort :: Ord a => [[a]] -> [[a]]
specialSort lsts = quickSortOn mostFrequent lsts
  where mostFrequent lst = fst $ head $ quickSortBy (\p1 p2 -> snd p1 < snd p2) $ histogram lst
