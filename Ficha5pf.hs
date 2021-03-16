-- 1)
-- a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = f x || any' f xs 

any'' :: (a -> Bool) -> [a] -> Bool
any'' f [] = False
any'' f (x:xs) | f x = True
               | otherwise = any'' f xs
-- b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f l1 [] = []
zipWith' f [] l2 = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- c)
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = [] 

-- d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = x:xs 

-- f)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (x:xs) | f x = (x:a,b)
               | otherwise = ([],(x:xs))
         where (a,b) = span' f xs

-- g)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f y [] = []
deleteBy' f y (x:xs) | f y x = xs
                     | otherwise = x : deleteBy' f y xs

-- h)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f [x] = [x]
sortOn f (x:xs) = (sortOn f lowers) ++ [x] ++ (sortOn f uppers)
   where (lowers,uppers) = parte f x xs

parte :: Ord b => (a -> b) -> a -> [a] -> ([a],[a])
parte f y [] = ([],[])
parte f y (x:xs) | (f x) < (f y)  = (x:a,b)
                 | otherwise = (a,x:b)
       where (a,b) = parte f y xs

-- 2)
type Polinomio = [Monomio] 
type Monomio = (Float,Int)

--a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau x l = filter (\(a,b) -> b == x) l 

--b)
conta :: Int -> Polinomio -> Int
conta g l = length (filter (\(a,b) -> b == g) l)

--c)
grau :: Polinomio -> Int
grau l = maximum (map snd l)

--d)
deriv :: Polinomio -> Polinomio
deriv l = map (\(a,b) -> ((fromIntegral b) * a,b - 1)) l

--e)
calcula :: Float -> Polinomio -> Float
calcula x l = sum (map (\(a,b) -> a * x^(fromIntegral b)) l)

calcula1 :: Float -> Polinomio -> Float
calcula1 x l = foldl (\ac (a,b) -> ac + a * (x^b)) 0 l

--f)
simp :: Polinomio -> Polinomio
simp l = filter (\(a,b) -> a/=0) l

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult m l = map (\(a,b) -> (a*fst m,b+snd m)) l

--h)
ordena :: Polinomio -> Polinomio
ordena l = sortOn snd l



myspan :: (a-> Bool) -> [a] -> ([a],[a])
myspan f [] = ([],[])
myspan f (x:xs) 
 | f x = (x:a , b)
 | otherwise = ([],(x:xs))
   where (a,b) = myspan f xs 


mydeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
mydeleteBy f y [] = []
mydeleteBy f y (x:xs)
 | f y x = xs
 | otherwise = x : mydeleteBy f y xs







mysortOn :: Ord b => (a -> b) -> [a] -> [a]
mysortOn f [] = []
mysortOn f [x] = [x]
mysortOn f (x:xs) 
 | f x <= minimum (map f xs) = x : mysortOn f xs
 | otherwise = mysortOn f (xs ++ [x])





