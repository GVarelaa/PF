
-- 1)
myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y | y >= x = x : myenumFromTo (x+1) y
                 | otherwise = []
-- 2
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z | z >= x = x : myenumFromThenTo (y) (2*y-x) z
                       | otherwise = []

-- 3)
(+++) :: [a] -> [a] -> [a]
(+++) [] l2 = l2
(+++) (x:xs) l2 = x : (+++) xs l2

-- 4)
(!!!) :: [a] -> Int -> a
(!!!) [] y = error "Empty list"
(!!!) (x:xs) y | y==0 = x
               | otherwise = (!!!) xs (y-1) 

-- 5)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 6)
take' :: Int -> [a] -> [a]
take' y [] = []
take' y (x:xs) | y == 0 = []
               | y>length (x:xs) = (x:xs)
               | otherwise = x : take' (y-1) xs

-- 7)
drop' :: Int -> [a] -> [a]
drop' y [] = []
drop' 0 (x:xs) = (x:xs)
drop' y (x:xs) 
        | y >length (x:xs) = []
        | otherwise = drop' (y-1) xs

-- 8)
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys


-- 9)
elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) | y==x = True
               | otherwise = elem' y xs


-- 10)
replicate' :: Int -> a -> [a]
replicate' 0 y = []
replicate' x y = y : replicate' (x-1) y

-- 11)
intersperce :: a -> [a] -> [a]
intersperce y [] = []
intersperce y [x] = [x]
intersperce y (x:xs) = x : y : intersperce y xs

--12)
group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (x:y:xs) | x /= y = [[x]] ++ group (y:xs)
               | otherwise = [groupmenor  x (x:y:xs)] ++ group ( subLista a b)
        where a = (x:y:xs) 
              b = groupmenor x (x:y:xs)

groupmenor :: Eq a => a -> [a] -> [a]
groupmenor a [] = []
groupmenor a (x:xs) | a==x = x : groupmenor a xs
                    | otherwise = [] 

subLista :: Eq a => [a] -> [a] -> [a]
subLista [] l2 = []
subLista l1 [] = l1
subLista (x:xs) (y:ys) | x==y = subLista xs ys

-- 13)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- 14)
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' (x:xs) = [] : aux x (inits' xs)
     where aux a (x:xs) = (a:x):(aux a xs)
           aux a [] = []

-- 15) t
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs):tails xs

--16)
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x==y && isPrefixOf xs ys 

--17)
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l1 l2 = last l1 == last l2 && isSuffixOf a b
      where a = take (length l1 - 1) l1
            b = take (length l2 - 1) l2

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l (x:xs) = l == (x:xs) || isSuffixOf' l xs

-- 18)
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) | x == y = isSubsequenceOf xs ys
                              | otherwise = isSubsequenceOf (x:xs) ys

-- 19)
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x [] = []
elemIndices y (x:xs) = aux 0 y (x:xs)
      where aux x y [] = []
            aux x y (z:zs) | y==z = x : aux (x+1) y zs
                           | otherwise = aux (x+1) y zs

-- 20)
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (auxRemove x xs)
   where auxRemove :: Eq a => a -> [a] -> [a]
         auxRemove y [] = []
         auxRemove y (x:xs) | y == x = auxRemove y xs
                            | otherwise = x : auxRemove y xs

-- 21)
delete :: Eq a => a -> [a] -> [a]
delete y [] = []
delete y (x:xs) | y==x = xs
                | otherwise = x : delete y xs

-- 22)
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) l1 [] = l1
(\\) (x:xs) (y:ys) = (\\) (auxDelete y (x:xs)) ys -- (\\) [2,2,3,4,5,1] [1,5]
   where auxDelete y [] = []
         auxDelete y (x:xs) | y==x = xs
                            | otherwise = x : auxDelete y xs

-- 23)
union :: Eq a => [a] -> [a] -> [a]
union l1 [] = l1
union [] l2 = l2
union (x:xs) (y:ys) | auxPertence y (x:xs) = union (x:xs) ys
                    | otherwise = union ((x:xs)++[y]) ys
    where auxPertence :: Eq a => a -> [a] -> Bool
          auxPertence y [] = False
          auxPertence y (x:xs) = y == x || auxPertence y xs

-- 24)
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] l2 = []
intersect l1 [] = []
intersect (x:xs) l2 | auxPertence x l2 = x : intersect xs l2
                    | otherwise = intersect xs l2
    where auxPertence :: Eq a => a -> [a] -> Bool
          auxPertence y [] = False
          auxPertence y (x:xs) = y == x || auxPertence y xs

-- 25)
insert :: Ord a => a -> [a] ->[a]
insert y [] = [y]
insert y (x:xs) | y <= x = y:x:xs
                | otherwise = x : insert y xs 

-- 26)
unwords' :: [String] -> String
unwords' [] = []
unwords' [x] = x
unwords' (x:xs) = x ++ " " ++ unwords' xs

-- 27)
unlines' :: [String] -> String
unlines' [] = []
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

-- 28) 
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (x:xs) = if x > (sel xs a) then 0
                  else a + 1
      where a = pMaior xs 

sel :: [a] -> Int -> a
sel (x:xs) 0 = x
sel (x:xs) y = sel xs (y-1)

-- 29)
temRepetidos :: Eq a => [a] -> Bool --temRepetidos [2,3,1,7,3,9]
temRepetidos [] = False
temRepetidos (h:t) | elem h t = True
                   | otherwise = temRepetidos t

-- 30)
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) | elem x ['1'..'9'] = x : algarismos xs
                  | otherwise = algarismos xs

-- 31)
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:xs) = y : posImpares xs

-- 32)
posPares :: [a] -> [a]
posPares [] = []
posPares [x] =[x]
posPares (x:y:xs) = x : posPares xs

-- 33)
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) | x <= y = isSorted (y:xs)
                  | otherwise = False

--34) insertion sort
iSort :: Ord a => [a] -> [a]
iSort [x] = [x]
iSort (x:xs) = insert x (iSort xs)


iSort' :: Ord a => [a] -> [a]
iSort' [x] = [x]
iSort' (x:xs)=  h : iSort' t
    where h = minimo (x:xs)
          t = remove h (x:xs)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) | x < minimo xs = x
        | otherwise = minimo xs

remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) | x == y = ys
        | otherwise = y : remove x ys

-- 25) 
--insert :: Ord a => a -> [a] -> [a]
--insert x [] = [x]
--insert y (x:xs) | y <= x = y : x : xs
--        | otherwise = x : insert y xs

qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = (qSort l1) ++ (qSort l2)
    where (l1,l2) = divide x xs

divide :: Ord a => a -> [a] -> ([a],[a])
divide _ [] = ([],[])
divide y (x:xs) | y > x = (x:a ++ [y],b)
                | otherwise = (a,x:b) 
    where (a,b) = divide y xs

-- 35)
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) = x <= y && menor xs ys 

-- 36)
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet y [] = False
elemMSet y ((a,b):xs) | y == a = True
                      | otherwise = False || elemMSet y xs

-- 37)
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,b):xs) = b + lengthMSet xs

-- 38)
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet (y:ys) = auxConverte y ++ converteMSet ys
   where auxConverte :: (a,Int) -> [a]
         auxConverte (a,0) = []
         auxConverte (a,x) = a : auxConverte (a,x-1)

-- 39)
insereMSet :: Eq a => a -> [(a,Int)] ->[(a,Int)]
insereMSet y [] = [(y,1)]
insereMSet y ((a,b):xs) | y==a = (a,b+1):xs
                        | otherwise = (a,b): insereMSet y xs

-- 40)
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet y [] = []
removeMSet y ((a,b):xs) | y == a && b-1 > 0 = (a,b-1) : xs
                        | y == a && b-1 == 0 = xs
                        | otherwise = (a,b) : removeMSet y xs

-- 41)
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) = (x , aux1) : constroiMSet a
    where (aux1, aux2) = conta x (x:xs)
          a = subtraiLista (x:xs) aux2
          conta :: Eq a => a -> [a] -> (Int,[a])
          conta y [] = (0,[])
          conta y (x:xs) | y == x = (1 + a, x : b)
                         | otherwise = conta y xs
              where (a,b) = conta y xs
          subtraiLista :: Eq a => [a] -> [a] -> [a]
          subtraiLista [] l2 = []
          subtraiLista l1 [] = l1
          subtraiLista (x:xs) (y:ys) | x==y = subtraiLista xs ys
                                     | otherwise = subtraiLista (x:xs) ys

-- 42) 
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):xs) = (a:as , bs)
          where (as,bs) = partitionEithers xs
partitionEithers ((Right b):xs) = (as , b:bs)
          where (as,bs) = partitionEithers xs

-- 43)
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a:xs) = a : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs

-- 44)
data Movimento = Norte | Sul | Este | Oeste
              deriving (Show)

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (a:as) = posicao (case a of Norte -> (x,y+1)
                                          Sul -> (x,y-1)
                                          Este -> (x+1,y)
                                          Oeste -> (x-1,y) ) as

posicao' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao' (x,y) [] = (x,y)
posicao' (x,y) (Norte:as) = posicao' (x,y+1) as
posicao' (x,y) (Sul:as) = posicao' (x,y-1) as
posicao' (x,y) (Este:as) = posicao' (x+1,y) as
posicao' (x,y) (Oeste:as) = posicao' (x-1,y) as

-- 45)
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | y2>y1 = Norte : caminho (x1,y1 + 1) (x2,y2)
                        | y2<y1 = Sul : caminho (x1,y1 - 1) (x2,y2)
                        | x2>x1 = Este : caminho (x1 +1,y1) (x2,y2)
                        | x2<x1 = Oeste : caminho (x1 -1,y1) (x2,y2)
                        | x1==x2 && y1==y2 = []

-- 46)
vertical :: [Movimento] -> Bool
vertical [] = False
vertical [Norte] = True
vertical [Sul] = True
vertical (Norte:xs) = True && vertical xs
vertical (Sul:xs) = True && vertical xs
vertical (Este:xs) = False
vertical (Oeste:xs) = False

-- 47)
data Posicao = Pos Int Int
          deriving Show

maisCentral :: [Posicao] -> Posicao 
maisCentral [x] = x
maisCentral (x:y:xs) | (auxDist x) > (auxDist y) = maisCentral (y:xs)
                     | otherwise = maisCentral (x:xs)
auxDist :: Posicao -> Int
auxDist (Pos a b) = a^2 + b^2

-- 48)
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos y [] = []
vizinhos b@(Pos x1 y1) (a@(Pos x2 y2):xs) | abs (x1-x2) == 1 && y1==y2 || abs (y1-y2) == 1 && x1==x2 = a : vizinhos b xs 
                                          | otherwise = vizinhos b xs

-- 49)
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada (a@(Pos x1 y1):b@(Pos x2 y2):xs) = y1==y2 && mesmaOrdenada (b:xs)

-- 50)
data Semaforo = Verde | Amarelo | Vermelho
        deriving Show

--interseccaoOK :: [Semaforo] -> Bool
--interseccaoOK 

