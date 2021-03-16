-- 1
-- a)
data ExpInt = Const Int | Simetrico ExpInt | Mais ExpInt ExpInt | Menos ExpInt ExpInt | Mult ExpInt ExpInt

calcula :: ExpInt -> Int
calcula (Const x)     = x
calcula (Simetrico x) = -1 * (calcula x)
calcula (Mais x y)    = (calcula x) + (calcula y)
calcula (Menos x y)   = (calcula x) - (calcula y)
calcula (Mult x y)    = (calcula x) * (calcula y)

-- b)
infixa :: ExpInt -> String
infixa (Const x)     = show x
infixa (Simetrico x) = "( -1 * " ++ infixa x ++ ")"
infixa (Mais x y)    = "(" ++ infixa x ++ " + " ++ infixa y ++ ")"
infixa (Menos x y)   = "(" ++ infixa x ++ " - " ++ infixa y ++ ")"
infixa (Mult x y)    = "(" ++ infixa x ++ " * " ++ infixa y ++ ")"

-- c)
posfixa :: ExpInt -> String
posfixa (Const x ) = show x
posfixa (Simetrico x) = "-" ++ posfixa x
posfixa (Mais x y) = posfixa x ++ " " ++ posfixa y ++ " +"
posfixa (Menos x y) = posfixa x ++ " " ++ posfixa y ++ " -"
posfixa (Mult x y) = posfixa x ++ " " ++ posfixa y ++ " *"

-------------------------
data RTree a = R a [RTree a]

rtree1 = R 6 [R 4 [R 7 [R 1 [],
                        R 3 []],
                   R 9 []],
              R 3 [R 12 []],
              R 6 [],
              R 11 []]

-- 2
-- a)
soma :: Num a => RTree a -> a
soma (R a l) = a + sum (map soma l) 

-- b)
altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l)  = 1 + maximum (map altura l)

-- c)
prune :: Int -> RTree a -> RTree a
prune x (R a l) 
 | x == 0 = R a l
 | otherwise = R a (map (prune (x-1)) l)

-- d)
mirror :: RTree a -> RTree a
mirror (R a l) = R a (map mirror (reverse l))

-- e)
postorder :: RTree a -> [a]
postorder (R a l) = concat (map postorder l) ++ [a]

-- 3
data LTree a = Tip a | Fork (LTree a) (LTree a) deriving (Show)

ltree1 = Fork (Fork (Tip 5) (Fork (Tip 6) (Tip 4))) (Fork (Fork (Tip 3) (Tip 7)) (Tip 5))

-- a)
ltSum :: Num a => LTree a -> a
ltSum (Tip a)    = a
ltSum (Fork e d) = ltSum e + ltSum d

-- b)
listaLT :: LTree a -> [a]
listaLT (Tip a)    = [a]
listaLT (Fork e d) = listaLT e ++ listaLT d

-- c)
ltHeight :: LTree a -> Int
ltHeight (Tip a)    = 0
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

-- 4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving (Show)
data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)

ftree1 = No 8 (No 1 (Leaf 5)
                    (No 2 (Leaf 6)
                          (Leaf 4)))
              (No 9 (No 10 (Leaf 3)
                           (Leaf 7))
                    (Leaf 5))

-- a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf a) = (Empty, Tip a)
splitFTree (No a e d) = (Node a e1 d1 , Fork e2 d2 )
                where (e1,e2) = splitFTree e
                      (d1,d2) = splitFTree d

-- b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip a)             = Just (Leaf a)
joinTrees (Node a e1 d1) (Fork e2 d2) = Just (No a e3 d3)
                where Just e3 = joinTrees e1 e2
                      Just d3 = joinTrees d1 d2

















