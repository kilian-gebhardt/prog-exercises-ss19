-- product of squares of even numbers 

pse :: [Int] -> Int
pse xs = foldr (*) 1 (map (\x -> x * x) (filter even xs))

pse' xs = foldr (*) 1 $ map (^2) $ filter even xs
pse'' = foldr (*) 1 . map (^2) . filter even

square :: Int -> Int
square x = x * x
