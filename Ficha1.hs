module Ficha1 where

import Data.Char

-- 1)
-- a)
perimetro :: Double -> Double
perimetro x = 2 * pi * x

-- b)
dist :: (Double, Double) -> (Double, Double) -> Double
dist (a,b) (c,d) = sqrt ((d-b) ^ 2 + (c-a) ^ 2 )

--c)
primUlt :: [Float] -> (Float, Float)
primUlt list = (head (list),  last (list) )

--d)
multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

--e)
truncaImpar :: [Float] -> [Float] 
truncaImpar list = if odd (length list) then tail list else list

--f)
max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y

--g)
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

--2 
-- a)
nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c = if ( (b ^ 2) - 4 * a * c ) < 0 then 0
                 else if ( (b ^ 2) - 4 * a * c ) > 0 then 2
                    else 1

-- b)
raizes :: Float -> Float -> Float -> [Float]
raizes a b c = if nRaizes a b c == 2 then [(-b + sqrt(b^2 - 4*a*c))/(2*a), (-b - sqrt(b^2 -4*a*c))/(2*a)] 
                else if nRaizes a b c == 1 then [-b/(2*a)] 
                    else []
{-}
-- 3 a)
type Hora = (Int,Int)
testarHora :: Hora -> Bool
testarHora (x,y) = (x>=0 && x<24) && (y>=0 && y<60)

-- 3 b)
testarHora2 :: Hora -> Hora -> Bool
testarHora2 (x1,y1) (x2,y2) = x1>x2 || (x1==x2 && y1>y2)
--3 b) v2
testarHora3 :: Hora -> Hora -> Hora
testarHora3 (x1,y1) (x2,y2) = if x1>x2 then (x1,y1)
                                else if (x1==x2 && y1>y2) then (x1,y1)
                                    else (x2,y2)

-- 3 c)
converterHoras :: Hora -> Int
converterHoras (x,y) = x * 60 + y

-- 3 d)
converterMinutos :: Int -> Hora
converterMinutos x = (div x 60,mod x 60)

--3 e)
dHoras :: Hora -> Hora -> Int
dHoras (x1,y1) (x2,y2) = (abs(x2-x1) * 60) + abs(y2-y1)

--3 f)
addMins :: Hora -> Int -> Hora
addMins (x,y) z = if (y + mod z 60) > 60 then (x + div z 60 + 1, y + mod z 60 - 60)
                    else (x + div z 60, y + mod z 60)

--ou

addMins2 :: Hora -> Int -> Hora
addMins2 (h,m) a 
                |(m + a) < 60 = (h , m + a)
                |otherwise =(h + div (m+a) 60, mod (m+a) 60) -}


data Hora = H Int Int
    deriving (Show,Eq)

-- 4 a)

th :: Hora -> Bool
th (H x y) = (x>=0 && x<24) && (y>=0 && y<60)

-- b)
th2 :: Hora -> Hora -> Bool
th2 (H x1 x2) (H y1 y2) | x1 > y1 = True
                        | x1 == y1 && x2>y2 = True
                        | otherwise = False

-- c)
converterH :: Hora -> Int
converterH (H x y) = x * 60 + y

-- d)
converterM :: Int -> Hora
converterM x = H (div x 60) (mod x 60) 

-- e)
diferencaH :: Hora -> Hora -> Int
diferencaH h1 h2 = abs((converterH h1) -(converterH h2))

-- f)
addM :: Int -> Hora -> Hora
addM x h = converterM (x + converterH h) 







data Semaforo = Verde | Amarelo | Vermelho
    deriving (Show, Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

--ou

next3 :: Semaforo -> Semaforo
next3 x   | x == Verde = Amarelo
          | x == Amarelo = Vermelho
          | x == Vermelho = Verde

-- ou

next2 :: Semaforo -> Semaforo
next2 x = if x == Verde then Amarelo
            else if x == Amarelo then Vermelho
                else Verde
-- b)
stop :: Semaforo -> Bool
stop x = if x == Verde then False
          else True

--ou

stop2 :: Semaforo -> Bool
stop2 Vermelho = True
stop2 Verde = False
stop2 Amarelo = True

-- c)
safe :: Semaforo -> Semaforo -> Bool
safe x y = if x == Vermelho || y == Vermelho then True
            else if ((x == Verde && y == Verde) || (x == Verde && y == Amarelo) || (x == Amarelo && y == Verde)) then False
              else False


data Ponto = Cartesiano Double Double | Polar Double Double
    deriving (Show,Eq)

-- a)
posx :: Ponto -> Double
posx (Cartesiano x y) = abs(x)
posx (Polar x y) = abs(x * cos y)

-- b)
posy :: Ponto -> Double
posy (Cartesiano x y) = abs(y)
posy (Polar x y) = (x * sin y)

-- c)
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar x y) = abs(x)

-- d)
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan(abs(y)/abs(x))
angulo (Polar x y) = y

-- e)
dist' :: Ponto -> Ponto -> Double
dist' ponto1 ponto2 = sqrt((posx(ponto1)-posx(ponto2))^2+((posy(ponto1)-posy(ponto2))^2))

-- 7 a)

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto
    deriving(Show,Eq)

-- a)
poligono :: Figura -> Bool
poligono (Circulo c r) = False
poligono (Rectangulo p1 p2) = (posx(p1)/=posx(p2)) && (posy(p1)/=posy(p2))
poligono (Triangulo p1 p2 p3)
                             |(posx p1 - posx p2 == 0 && posx p2 - posx p3 == 0) && (posx p1 - posx p3 == 0) = False
                             |otherwise = (posy(p2) - posy(p1))/(posx(p2) - posx(p1)) /= (posy(p2) - posy(p3))/(posx(p2) - posx(p3))


-- x = cosy1 * x1
--b)
vertices :: Figura -> [Ponto]
vertices (Circulo c r) = []
vertices (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = if poligono (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) then [(Cartesiano x1 y1), (Cartesiano x2 y2), (Cartesiano x1 y2), (Cartesiano x2 y1)]
                                                                 else []


vertices (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) | poligono (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) = [(Cartesiano x1 y1), (Cartesiano x2 y2), (Cartesiano x3 y3)]
                                                                              | otherwise = []
-- c)
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist' p1 p2
        b = dist' p2 p3
        c = dist' p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- fórmula de Heron

area (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = (dist' a b) * (dist' b c)
                                                        where a = (Cartesiano x1 y1)
                                                              b = (Cartesiano x2 y1)
                                                              c = (Cartesiano x2 y2)
area (Circulo _ r) = pi * r^2

perimetro' :: Figura -> Double
perimetro' (Circulo _ r) = 2 * pi * r
perimetro' (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = 2 * abs (x2 - x1) + 2 * abs (y2 - y1)
perimetro' (Triangulo p1 p2 p3) = (dist' p1 p2) + (dist' p2 p3) + (dist' p3 p1)

--8 a)
isLower2 :: Char -> Bool
isLower2 x = elem (ord x) [97..122]
--ou
isLower3 :: Char ->Bool
isLower3 x | ord x >= 97 && ord x <= 122 = True
           | otherwise = False


-- b)
isDigit2 :: Char -> Bool
isDigit2 x = elem (ord x) [48..57]

-- c)

isAlpha2 :: Char -> Bool
isAlpha2 x = elem (ord x) [65..90] || elem (ord x) [97..122]

-- d)
toUpper2 :: Char -> Char
toUpper2 x | isLower2 x = chr((ord x) - 32)
           | otherwise = error "introduza uma letra minuscula"

-- e)
intToDigit2 :: Int -> Char
intToDigit2 x | elem x [0..9] = chr(x + 48)
              | otherwise = error "Not a digit bro"

-- f)
digitToInt2 :: Char -> Int
digitToInt2 x | elem (ord x) [48..57] = (ord x) - 48
              | otherwise = error "Introduza um digito"

