data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)
-- 1

-- (Node 2 Empty (Node 7 Empty Empty))


arvore1 = (Node 5 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 9 (Node 7 (Node 6 Empty Empty) (Node 8 Empty Empty)) Empty))

turma1 :: Turma
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty) 
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                        (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))
-- a)
altura :: BTree a -> Int
altura Empty        = 0
altura (Node a b c) = 1 + max (altura b) (altura c) 

-- b)
contaNodos :: BTree a -> Int
contaNodos Empty        = 0
contaNodos (Node a e d) = 1 + contaNodos e + contaNodos d

-- c)
folhas :: BTree a -> Int
folhas Empty                = 0
folhas (Node _ Empty Empty) = 1
folhas (Node a e d)         = folhas e + folhas d

-- d)
prune :: Int -> BTree a -> BTree a 
prune x Empty = Empty
prune x (Node a e d) 
 | x == 0     = Empty 
 | otherwise  = Node a (prune (x-1) e) (prune (x-1) d)

-- e)
path :: [Bool] -> BTree a -> [a]
path _ Empty         = []
path [] (Node a e d) = [a]
path (x:xs) (Node a e d)
 | x == False        = [a] ++ path xs e
 | otherwise         = [a] ++ path xs d 

-- f)
mirror :: BTree a -> BTree a
mirror Empty        = Empty
mirror (Node a e d) = Node a (mirror d) (mirror e)

-- g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node a1 e1 d1) (Node a2 e2 d2) = Node (f a1 a2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)

-- h)
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) = (Node a x1 y1, Node b x2 y2, Node c x3 y3) 
          where (x1,x2,x3) = unzipBT e
                (y1,y2,y3) = unzipBT d

-- 2
-- a)
minimo :: Ord a => BTree a -> a
minimo (Node a Empty _) = a
minimo (Node a e d) = minimo e

-- b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty _) = Empty
semMinimo (Node a e d) = Node a (semMinimo e) d

-- c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node a Empty _) = (a,Empty)
minSmin (Node a e d) = (x,Node a y d)
           where (x,y) = minSmin e

-- d)
remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node a Empty Empty)
 | x == a = Empty
 | otherwise = Node a Empty Empty
remove x (Node a Empty d ) = Node a Empty (remove x d)
remove x (Node a e Empty ) = Node a (remove x e) Empty 
remove x (Node a e d)
 | x == a = Empty
 | x == fst (minSmin e) = Node a (snd (minSmin e)) d
 | x == fst (minSmin d) = Node a e (snd (minSmin d))
 | otherwise = Node a (remove x e) (remove x d)

-- 3
-- a)
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL  deriving Show
data Classificacao = Aprov Int| Rep| Faltou deriving Show
type Turma = BTree Aluno  --  arvore binaria de procura (ordenada por numero)

inscNum :: Numero -> Turma -> Bool
inscNum num Empty = False 
inscNum num (Node (n,nome,r,cl) e d)
 | num == n = True
 | otherwise = inscNum num e || inscNum num d

-- b)
inscNome :: Nome -> Turma -> Bool
inscNome nome Empty = False
inscNome nome (Node (n,nom,r,cl) e d) = nome == nom || inscNome nome e || inscNome nome d

-- c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nr,nome,TE,cl) e d) = trabEst e ++ [(nr,nome)] ++ trabEst d
trabEst (Node a e d) = trabEst e ++ trabEst d

-- d)
nota :: Numero -> Turma -> Maybe Classificacao
nota num Empty = Nothing
nota num (Node (nr,nome,r,cl) e d) 
 | num == nr = Just cl
 | num > nr = nota num d
 | otherwise = nota num e 

-- f)
percFaltas :: Turma -> Float
percFaltas t = (numFaltas t / fromIntegral (contaNodos t)) * 100

numFaltas :: Turma -> Float
numFaltas Empty = 0
numFaltas (Node (nr,nome,r,Faltou) e d) = 1 + numFaltas e + numFaltas d
numFaltas (Node a e d) = numFaltas e + numFaltas d

-- g)
mediaAprov :: Turma -> Float
mediaAprov t = somaAprov t / numAlunosAprov t

somaAprov :: Turma -> Float
somaAprov Empty = 0
somaAprov (Node (nr,nome,r,Aprov x) e d) = (fromIntegral x) + somaAprov e + somaAprov d
somaAprov (Node a e d) = somaAprov e + somaAprov d

numAlunosAprov :: Turma -> Float
numAlunosAprov Empty = 0
numAlunosAprov (Node (nr,nome,r,Aprov x) e d) = 1 + numAlunosAprov e + numAlunosAprov d
numAlunosAprov (Node a e d) = numAlunosAprov e + numAlunosAprov d

-- h)
aprovAv :: Turma -> Float
aprovAv t = x / y
	where (x,y) = aux t

aux :: Turma -> (Float,Float)
aux Empty = (0,0)
aux (Node (nr,nome,r,Aprov x) e d) = (1 + y, 1 + z)
         where (y,z) = (y1+y1,z1+z2) 
               (y1,z1) = aux e
               (y2,z2) = aux d
aux (Node a e d) = (y, 1 + z)
         where (y,z) = (y1+y2,z1+z2)
               (y1,z1) = aux e
               (y2,z2) = aux d



