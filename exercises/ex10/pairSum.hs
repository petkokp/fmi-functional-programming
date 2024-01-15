pairSum :: (Ord b, Num b) => b -> [b] -> [(b, b)]
pairSum n lst = [(x, y) | x <- lst, y <- lst, x <= y, x + y == n]