module Ficha3 where

import Ficha1

--data Hora = H Int Int
--        deriving (Show) 

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- 1
-- a)   
testarEtapa :: Etapa -> Bool
testarEtapa (x,y) = th x && th y && th2 y x

-- b)
testarViagem :: Viagem -> Bool
testarViagem [(x,y)] = th2 y x
testarViagem ((h1,h2):(h3,h4):xs) | (th2 h2 h1) && (th2 h3 h2) = True && testarViagem ((h3,h4):xs)
                                  | otherwise = False

-- c)
calcularPartidaeViagem :: Viagem -> Etapa
calcularPartidaeViagem [(h1,h2)] = (h1,h2)
calcularPartidaeViagem (x:xs) = (fst x,snd(last xs))

--ou
calcularPartidaeViagem' :: Viagem -> Etapa
calcularPartidaeViagem' [(h1,h2)] = (h1,h2)
calcularPartidaeViagem' [(h1,h2),(h3,h4)] = (h1,h4)
calcularPartidaeViagem' ((h1,h2):(h3,h4):xs) = calcularPartidaeViagem ((h1,h2):xs) 

-- d)
tempoViagem :: Viagem -> Int
tempoViagem [] = 0
tempoViagem ((h1,h2):xs) = diferencaH h2 h1 + tempoViagem xs

-- e)
tempoEspera :: Viagem -> Int
tempoEspera [] = 0
tempoEspera [(h1,h2),(h3,h4)] = diferencaH h3 h2
tempoEspera ((h1,h2):(h3,h4):xs) = diferencaH h3 h2 + tempoEspera ((h3,h4):xs)

-- f)
tempoTotal :: Viagem -> Int
tempoTotal x = tempoViagem x + tempoEspera x

-- 2
type Poligonal = [Ponto]
-- a)
comprimentoLinha :: Poligonal -> Double
comprimentoLinha [x] = 0
comprimentoLinha (x:y:xs) = dist' x y + comprimentoLinha (y:xs)

-- b)
testarLinha :: Poligonal -> Bool
testarLinha [x] = False
testarLinha (x:xs) | posx x == posx (last xs) && posy x == posy (last xs) = True
                   | otherwise = False

testarLinha' :: Poligonal -> Bool
testarLinha' [x1] = False
testarLinha' [x1,x2] = False
testarLinha' [x1,x2,x3] = x1==x3
testarLinha' (x1:x2:x3:xs) = testarLinha (x1:x3:xs)

-- c)
triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] | testarLinha [p1,p2,p3] = [Triangulo p1 p2 p3]
                     | otherwise = error "Introduza uma linha poligonal fechada"
triangula (p1:p2:p3:xs) | testarLinha (p1:p2:p3:xs) = Triangulo p1 p2 p3 : triangula (p1:p3:xs)
                        | otherwise = error "Introduza uma linha poligonal fechada"

-- d)
areaLinhafechada :: Poligonal -> Double
areaLinhafechada [p1,p2,p3] = area (Triangulo p1 p2 p3)
areaLinhafechada (p1:p2:p3:xs) = area (Triangulo p1 p2 p3) + areaLinhafechada (p1:p3:xs) 

-- e)
mover :: Poligonal -> Ponto -> Poligonal
mover [] y = [y]
mover (x:xs) y = y:x:xs

-- f)
zoom :: Double -> Poligonal -> Poligonal -- wrong
zoom y [p1, Cartesiano a b] = [p1, Cartesiano (a*y) (b*y)]
zoom y (x:Cartesiano a b:xs) = x: zoom y (Cartesiano (a*y) (b*y):xs)

-- 3
data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String 
          deriving Show
type Nome = String
type Agenda = [(Nome,[Contacto])]
-- a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail a b l1 = l1 ++ [(a,[Email b])]

-- b)
{-verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome [(x,y)] | nome == x = Just (auxEmails y)
                       | otherwise = Nothing
verEmails nome1 ((nome2,a):xs) | nome1 == nome2 = Just (auxEmails a)
                               | otherwise = verEmails nome1 xs

auxEmails :: [Contacto] -> [String]
auxEmails [] = []
auxEmails (x:xs) = case x of Email a   -> ([a] ++ auxEmails xs)
                             otherwise -> auxEmails xs
-}

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((nome,x):xs) | n == nome = Just (getEmails x) 
                          | otherwise = verEmails n xs

getEmails :: [Contacto] -> [String]
getEmails [] = []
getEmails ((Email x) : xs) = x : getEmails xs
getEmails (x:xs) = getEmails xs

-- c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (x:xs) = case x of Casa a    -> [a] ++ consTelefs xs
                              Tlm a     -> [a] ++ consTelefs xs
                              Trab a    -> [a] ++ consTelefs xs
                              otherwise -> consTelefs xs

-- d)
casa :: Nome -> Agenda -> Maybe Integer
casa nome1 [] = Nothing
casa nome1 ((nome2,(x:xs)):ys) | nome1 == nome2 = case x of Casa a    -> Just a
                                                            otherwise -> casa nome1 ((nome2,xs):ys)
                               | otherwise = casa nome1 ys

-- 4)
type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

data Data = D Dia Mes Ano
       deriving Show

type TabDN = [(Nome,Data)]

-- a)
procura :: Nome -> TabDN -> Maybe Data
procura nome  [] = Nothing
procura nome ((x,y):xs) | nome == x = Just y
                        | otherwise = procura nome xs

-- b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade d nome [] = Nothing
idade (D a1 b1 c1) nome ((x,(D a2 b2 c2)):xs) | nome == x && c2>c1 = Nothing
                                              | nome == x = Just (c1 - c2)
                                              | otherwise = idade (D a1 b1 c1) nome xs

-- c)
anterior :: Data -> Data -> Bool
anterior (D a1 b1 c1) (D a2 b2 c2) | c1<c2 || (c1==c2 && b1<b2) || (c1==c2 && b1==b2 && a1<a2) = True
                                   | otherwise = False

-- d)
ordena :: TabDN -> TabDN
ordena [x] = [x]
ordena ((nome1,d1):xs) = insere (nome1,d1) (ordena xs)              -- insertion sort insere um elemento numa lista ordenada
        where insere :: (Nome,Data) -> TabDN -> TabDN
              insere (n2,d2) [] = [(n2,d2)]
              insere (n2,d2) ((n1,d1):xs) | anterior d2 d1 = (n2,d2):(n1,d1):xs
                                          | otherwise = (n1,d1) : insere (n2,d2) xs

-- e)
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade d [] = []
porIdade d1 ((n,d2):xs) | d1 == d2 = []
             where 
equiData :: Data -> Data -> Bool
equiData (D x1 y1 z1) (D x2 y2 z2) = x1 == x2 && y1 == y2 && z1==z2




