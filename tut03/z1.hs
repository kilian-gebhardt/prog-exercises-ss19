maxLength :: [[Int]] -> Int
-- maxLength xs = maximum $ map length xs
maxLength [] = 0
maxLength (x:xs) = let l = length x
                       m = maxLength xs
                   in if l < m then m
                      else l
