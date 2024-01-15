isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf l@(x:xs) (y:ys) = x == y && isInfixOf xs ys || isInfixOf l ys