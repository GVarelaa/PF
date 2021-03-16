import Data.Char

--2 
-- d)
ex2d = [take n [1,1..]| n <- [1..5]]    
--[replicate n 1 | n <- [1..5]]

--e)
ex2e = [ product [1..x] | x <- [1..6]]

--3)
digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (x:xs) | isDigit x = (l1,x:l2) 
                  | isAlpha x = (x:l1,l2)
                  | otherwise = (l1,l2)
          where (l1,l2) = digitAlpha xs

digitAlpha' :: String -> (String,String)
digitAlpha' s = (filter (\x -> isAlpha x) s, filter (\x -> isDigit x) s)


--4)
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs) | x < 0 = (1+l1,l2,l3)
           | x > 0 = (l1,l2,1+l3)
           | x == 0 = (l1,1+l2,l3)
      where (l1,l2,l3) = nzp xs

-- 5)
divMod' :: Integral a => a -> a -> (a, a)
divMod' x y = (div x y, x - (div x y)*y)

divMod'' x y | x < y = (0,x)
             | otherwise = (1+l1,l2)
    where (l1,l2) = divMod'' (x-y) y



--6)

fromD :: [Int] -> Int
fromD [] = 0
fromD l = snd (fromDigits' l)

fromDigits :: [Int] -> (Int,Int)
fromDigits [] = (0,0)
fromDigits (x:xs) = (tamanho + 1, x*10^tamanho + resto)
    where (tamanho, resto) = fromDigits xs  

-- 8)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n 

fibOtimizado :: Int -> Int
fibOtimizado x = fibAux x (0,1)


fibAux :: Int -> (Int,Int) -> Int
fibAux 0 (nesimo,prox) = nesimo
fibAux n (nesimo, prox) = fibAux (n-1) (prox, nesimo+prox)














