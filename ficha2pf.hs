
import Data.Char

--1 a)
-- funA [2,3,5,1] = 2^2 + funA [3,5,1] = 4 + 3^2 + funA [5,1] = 13 + 5^2 + funA [1] = 38 + 1^2 + funA [] = 39 + 0 = 39

-- b)
-- funB [8,5,12] = 8 : funB [5,12] = 8 : funB [12] = 8 : 12 : funB[] = 8 : 12 :[] = [8,12]

-- c)
-- funC [1,2,3,4,5] = funC [3,4,5] = funC [5]= []

--d)

--2 a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = 2*x : dobros xs

-- b)
numOcorre :: Char -> String -> Int
numOcorre y [] = 0
numOcorre y (x:xs) | y == x = 1 + numOcorre y xs
				   | otherwise = numOcorre y xs

-- c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (x:xs) | x>0 = True && positivos xs
				 | otherwise = False

-- d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) | x<=0 = soPos xs
			 | otherwise = x : soPos xs

-- e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) | x<0 = x + somaNeg xs
			   | otherwise = somaNeg xs

-- f)
{- tresUlt :: [a] -> [a]
tresUlt (a:b:c:[]) = (a:b:c:[])
tresUlt l | length l < 3 = l
	   	  | length l >= 3 = tresUlt (a:b:c:[]) -}


--tresUlt :: [a] -> [a]
--tresUlt l = case l of (a:b:c:d:xs) -> tresUlt (b:c:d:xs)
 --                     otherwise -> l

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) | length (x:xs) <= 3 = (x:xs)
 				| length xs > 3 = tresUlt xs
 				| otherwise = xs

-- g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (x:xs) = snd x : segundos xs

--h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros y [] = False
nosPrimeiros y ((a,b):xs) | y == a = True || nosPrimeiros y xs
						  | otherwise = nosPrimeiros y xs


-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(x,y,z)] = (x,y,z)
sumTriplos ((x,y,z):(a,b,c):xs) = sumTriplos ((x+a,y+b,z+c):xs)

-- 3 a)
soDigitos :: [Char] -> [Char]
soDigitos [] = ""
soDigitos (x:xs) | ord x>= 49 && ord x<= 57 = x : soDigitos xs
			     | otherwise = soDigitos xs
-- b) 
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) | elem (ord x) [97..122] = 1 + minusculas xs
				  | otherwise = minusculas xs 

-- c)
nums :: String -> [Int]
nums [] = []
nums (x:xs) | elem (ord x) [48..57] = ( ord (x) - 48: nums xs)
			| otherwise = nums xs

-- 4 a)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((a,b):xs) | n == b = 1 + conta n xs
				   | otherwise = conta n xs

-- b)
grau :: Polinomio -> Int
grau [] = 0
grau ((a,b):xs) | b > grau xs = b
				| otherwise = grau xs

-- c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau z [] = []
selgrau z (x:xs) | z == snd x = x : selgrau z xs
				 | otherwise = selgrau z xs

-- d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):xs) | b > 0 = (a * fromIntegral b, b-1) : deriv xs
				 | otherwise = (0,0) : deriv xs

-- e)
calcula :: Float -> Polinomio -> Float
calcula y [] = 0
calcula y ((a,b):xs) = a*(y^b) + calcula y xs

-- f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):xs) | a == 0 = simp xs
				| otherwise = (a,b) : simp xs

-- g) 
mult :: Monomio -> Polinomio -> Polinomio
mult (a1,b1) [] = []
mult (a1,b1) ((a2,b2):xs) = (a1*a2, b1+b2) : mult (a1,b1) xs

-- h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(a1,b1)] = [(a1,b1)]
normaliza ((a1,b1):(a2,b2):xs) | b1==b2 = normaliza ((a1+a2,b1) : xs)
							   | conta b1 xs == 0 = (a1,b1) : normaliza ((a2,b2):xs)
							   | otherwise = normaliza ((a1,b1) : xs ++ [(a2,b2)])

-- i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

-- j)
produto :: Polinomio -> Polinomio -> Polinomio
produto [] l = []
produto _ [] = []
produto ((a,b):xs) l =normaliza (mult (a,b) l ++ produto xs l)

-- k)
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b):xs) | b == grau((a,b):xs) = normaliza (ordena xs ++ [(a,b)])
				  | otherwise = ordena (head xs : tail xs ++ [(a,b)]) 

-- l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena p1 == ordena p2

































