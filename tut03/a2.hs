import Prelude hiding (even)

data Tree = Node Int [Tree]

noLeaves :: Tree -> Int
noLeaves (Node _ []) = 1
--noLeaves (Node _ ts) = sum (map noLeaves ts)
noLeaves (Node _ ts) = go ts
  where go :: [Tree] -> Int
        go []     = 0
        go (t:ts) = noLeaves t + go ts

-- falsch, zählt alle Knoten des Baums:
--noLeaves (Node x (t:ts)) = noLeaves t + noLeaves (Node x ts)

even :: Tree -> Bool
even (Node _ []) = True -- optional
even (Node _ ts) | length ts `mod` 2 == 0 = go ts
  where go :: [Tree] -> Bool
        go [] = True
        go (t:ts) = even t && go ts
even (Node _ ts) | otherwise = False

-- alternative Lösung:
even' (Node _ []) = True
even' (Node _ (x:y:zs)) = even' x && even' y && even' (Node 0 zs)
even' _ = False

