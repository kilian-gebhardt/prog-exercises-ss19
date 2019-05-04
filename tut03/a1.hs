data Tree = Node Int Tree Tree | Nil deriving (Show)

insertOne :: Tree -> Int -> Tree
insertOne Nil          x = Node x Nil Nil
insertOne (Node y l r) x | x < y     = Node y (insertOne l x) r
                         | otherwise = Node y l (insertOne r x)

insert :: Tree -> [Int] -> Tree
insert t []     = t
insert t (x:xs) = insert (insertOne t x) xs

inorder :: Tree -> [Int]
inorder Nil = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

eq :: Tree -> Tree -> Bool
eq Nil Nil = True
eq (Node x1 l1 r1) (Node x2 l2 r2) = x1 == x2 && eq l1 l2 && eq r1 r2
eq _ _ = False

