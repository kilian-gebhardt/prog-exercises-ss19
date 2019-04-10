import Prelude hiding (rem)
{-
prod :: [Int] -> Int
prod []       =       -- leere Liste
prod [x]      =       -- Liste mit genau einem Element
prod (x:xs)   =       -- Liste mit mindestens einem Element
prod (x:y:zs) =       -- Liste mit mindestens zwei Elementen
-}

prod :: [Int] -> Int
prod [] = 1
prod [x] = x              --optional
prod (x:xs) = x * prod xs

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rem :: Int -> [Int] -> [Int]
rem x [] = []
rem x (y:ys) = if x == y then rem x ys
               else y : rem x ys  

rem' :: Int -> [Int] -> [Int]
rem' x = filter (/=x)

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x] = True
isOrd (x:y:zs) = if x <= y then isOrd (y:zs)
                 else False

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

fibs :: [Int]
fibs = 1 : 1 : go 1 1
  where go :: Int -> Int -> [Int]
        go x y = (x+y): go y (x+y)


