type Drug = (String, [(String, Int)])

getDrugName :: Drug -> String
getDrugName (name, _) = name

getDrugIngredients :: Drug -> [(String, Int)]
getDrugIngredients (_, ingredients) = ingredients

l :: [Drug]
l = [("A", [("p", 5), ("q", 3)]), ("B", [("p", 4), ("q", 3)]), ("C", [("p", 3)])]

findIngredient :: (Eq a) => a -> [(a, b)] -> (a, b)
findIngredient ingredientName ingredients = head (filter (\x -> fst x == ingredientName) ingredients)

isStronger :: Drug -> Drug -> Bool
isStronger a b = all (\(name, quantity) -> snd (findIngredient name secondIngredients) <= quantity) (getDrugIngredients a) where secondIngredients = getDrugIngredients b