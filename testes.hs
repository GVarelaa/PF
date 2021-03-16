import System.Random
import Data.List

type Mat a = [[a]]

getElem :: Mat a -> IO a
getElem m = do let l = toList m
               k <- randomRIO (0,(length l) -1)
               let i = (!!) l k
               return i


toList :: Mat a -> [a]
toList [] = []
toList (x:xs) = x ++ toList xs



data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem] deriving (Show)

ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),Quadrado 4,Mover (4,3) (Quadrado 2)])

conta :: Imagem -> Int
conta (Quadrado i) = 1
conta (Mover _ i) = conta i
conta (Juntar l) = sum (map conta l)

substitui :: Int -> Imagem -> Imagem
substitui n (Quadrado i) = Juntar []
substitui n (Mover (x,y) i) = substitui n i
substitui n (Juntar [x]) = Juntar [substitui n x] 
substitui n (Juntar (x:y:xs))
 | n == conta x = Juntar ((substitui n x):y:xs)
 | otherwise = concatImagem x (substitui (n-conta x) (Juntar (y:xs)))

concatImagem :: Imagem -> Imagem -> Imagem
concatImagem x (Juntar l) = Juntar (x:l)


apaga :: Imagem -> IO Imagem
apaga imagem = do let l = conta imagem
                  k <- randomRIO (1,l)
                  let f = substitui k imagem
                  return f



type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l@(x:xs) = (x,last (aux l)) : geraconj (dropWhile (<= (last (aux l))) l)


aux :: [Int] -> [Int]
aux [] = []
aux [x] = [x]
aux [x,y] = if y == x+1 then [x,y] else [x]
aux (x:y:xs)
 | y == x+1 = x : aux (y:xs)
 | otherwise = [x]



data Contacto = Casa Integer| Trab Integer| Tlm Integer| Email String deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]

getContact :: Nome -> Agenda -> [Contacto]
getContact n [] = []
getContact n ((n1,l):xs)
 | n == n1 = l
 | otherwise = getContact n xs


consultaIO :: Agenda -> IO ()
consultaIO ag = do putStrLn "Introduza o nome:"
                   n <- getLine
                   if null (getContact n ag ) 
                    then do putStrLn "Nome inválido"
                            consultaIO ag
                    else print (getContact n ag )

ex1 :: Agenda
ex1 = [("Joao",[Email "fasdfas"]),("pedro",[Tlm 142341234])]

data RTree a = R a [RTree a] deriving (Show, Eq)

ex3 = R 1 [R 2 [], R 3 [R 4 [R 5 [], R 6 []]],R 7 []]

--paths :: RTree a -> [[a]]
--paths (R a []) = [[a]]
--paths (R a (x:xs)) = ([a] ++ paths x) : paths (R a xs)



movimenta :: IO (Int,Int)
movimenta = do aux5 (0,0)

aux10 :: (Int,Int) -> Char -> (Int,Int)
aux10 (x,y) 'n' = (x,y+1)
aux10 (x,y) 's' = (x,y-1)
aux10 (x,y) 'e' = (x+1,y)
aux10 (x,y) 'o' = (x-1,y)
aux10 (x,y) _ = (x,y)


aux5 :: (Int,Int) -> IO (Int,Int)
aux5 (x,y) = do k <- getChar
                if (k == 'n' || k == 's' || k == 'e' || k == 'o')
                    then do aux5 (aux10 (x,y) k)
                    else return (x,y)






