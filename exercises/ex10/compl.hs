complAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
complAdd (a, b) (c, d) = (a + c, b + d)

complSub :: (Int, Int) -> (Int, Int) -> (Int, Int)
complSub (a, b) (c, d) = (a - c, b - d)

complMul :: (Int, Int) -> (Int, Int) -> (Int, Int)
complMul (a, b) (c, d) = (a * c - b * d, a * d + b * c)
