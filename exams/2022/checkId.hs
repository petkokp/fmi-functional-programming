checkId :: (Foldable t, Eq a) => [a -> a] -> t a -> Bool
checkId fs xs = any (\(f, g) -> all (\x -> f (g x) == x) xs) [(f, g) | f <- fs, g <- fs]

