import System.Random
import Data.List


 
-- 1
-- a)

bingo :: IO ()
bingo = do 
 putStrLn "Bem-Vindos ao jogo do bingo"
 bingAux [] 



bingAux :: [Int] -> IO ()
bingAux l = do
    k <- getChar
    novoValor <- randomNaoRepetido (1,90) l           --randomRIO (1,90)
    putStr "Novo valor gerado: "
    print novoValor
    if (length (novoValor : l) < 90)
           then bingAux (novoValor : l)
           else putStrLn "Fim" 

randomNaoRepetido :: (Int,Int) -> [Int] -> IO Int
randomNaoRepetido gama lista = do
    novoValor <- randomRIO gama
    if (elem novoValor lista)
        then randomNaoRepetido gama lista
        else return (novoValor) 


-- b)

mastermind :: IO ()
mastermind = do
    putStrLn "mastermind"
    digitos <- geraDigitos    
    putStrLn "Números gerados!"
    print digitos
    guessing digitos

guessing :: [Int] -> IO ()
guessing l@[d1,d2,d3,d4] = do
    putStrLn "Insira 4 digitos"
    input <- getLine
    let listDigits = words input
    let listDigits'@[d1', d2', d3', d4'] = map read listDigits
    if (listDigits' == l) 
        then putStrLn "Ganhou"
            else do 
                let acertados   = length (filter (== True) (zipWith (==) l listDigits' ))
                let foraDoSitio = (calcForaSitio l (listDigits')) - acertados
                putStr "Acertados : "
                print acertados
                putStr "Acertados fora do sitio : "
                print foraDoSitio
                guessing l


calcForaSitio :: [Int] -> [Int] -> Int
calcForaSitio [] _ = 0                  
calcForaSitio (x:xs) l = if elem x l then 1 + calcForaSitio xs (delete x l)
                                        else calcForaSitio xs l



contaiguais :: [Int] -> [Int] -> Int
contaiguais [] [] = 0
contaiguais (x:xs) (y:ys)
 | x == y    = 1 + contaiguais xs ys
 | otherwise = contaiguais xs ys


geraDigitos :: IO [Int]
geraDigitos = do
    digito1 <- randomRIO (1,9)
    digito2 <- randomRIO (1,9)
    digito3 <- randomRIO (1,9)
    digito4 <- randomRIO (1,9)
    return [digito1, digito2, digito3, digito4]

-- 2
-- a
data Aposta = Ap [Int] (Int,Int) deriving (Show)

valida :: Aposta -> Bool
valida (Ap l (e1,e2)) = tamanhoValido && dentroAceites && semRepetidos
             where tamanhoValido = length l == 5
                   dentroAceites = e1 >= 1 && e1 <= 9 && e2 <= 9 && length l == length (filter (\x -> x>= 1 && x<= 50) l)
                   semRepetidos = e1 /= e2 && semRepetidosAux l
                   semRepetidosAux [] = True
                   semRepetidosAux (x:xs) = not (elem x xs) && semRepetidosAux xs

-- b
comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l (e1,e2)) (Ap lc (e1c,e2c)) = (numsCorretos, estrelasCorretas)
                where numsCorretos     = numsCorretosAux l lc
                      estrelasCorretas = (if e1 == e1c then 1 else 0) + (if e2 == e2c then 1 else 0) + (if e1 == e2c then 1 else 0) + (if e2 == e1c then 1 else 0) 
                      numsCorretosAux [] lc = 0
                      numsCorretosAux (x:xs) lc
                       | elem x lc = 1 + numsCorretosAux xs lc
                       | otherwise = numsCorretosAux xs lc

-- c
-- i
instance Eq Aposta where
    (==) ap1 ap2 = comuns ap1 ap2 == (5,2)

-- ii
premio :: Aposta -> Aposta -> Maybe Int
premio ap1 ap2 = case (comuns ap1 ap2) of
    (5,2) -> Just 1
    (5,1) -> Just 2
    (5,0) -> Just 3
    (4,2) -> Just 4
    (4,1) -> Just 5
    (4,0) -> Just 6
    (3,2) -> Just 7
    (2,2) -> Just 8
    (3,1) -> Just 9
    (3,0) -> Just 10
    (1,2) -> Just 11
    (2,1) -> Just 12
    (2,0) -> Just 13
    otherwise -> Nothing

-- d
-- i
leAposta :: IO Aposta
leAposta = do
    putStrLn "Insira uma aposta"
    line <- getLine
    let numbers = map read (words line)
    if ((length numbers) /= 7) 
                then do
                    putStrLn "Numero invalido de numeros?"
                    leAposta 
                else do
                  let [x1,x2,x3,x4,x5,x6,x7] = numbers         
                  let aposta = Ap [x1,x2,x3,x4,x5] (x6,x7)
                  if not (valida aposta)
                    then do
                       putStrLn "Aposta invalida?"
                       leAposta
                    else return aposta  

-- ii
joga :: Aposta -> IO () -- seta "<-"" tira o monad
joga chave = do
    aposta <- leAposta
    putStrLn ("O seu prémio é" ++ show (premio aposta chave)) 

-- e
geraChave :: IO Aposta
geraChave = do
    nums <- geraNums 5 []
    estrelas@[e1,e2] <- geraEstrelas 2 []
    return (Ap nums (e1,e2))

geraNums :: Int -> [Int] -> IO [Int]
geraNums 0 l = return []
geraNums x l = do
    n <- randomRIO (1,50)
    if (elem n l) then geraNums n l
                  else do 
                    ls <- geraNums (x-1) (n:l)
                    return (n:ls)

geraEstrelas :: Int -> [Int] -> IO [Int]
geraEstrelas 0 l = return []
geraEstrelas x l = do
    n <- randomRIO (1,9)
    if (elem n l) then geraEstrelas n l
                  else do 
                    ls <- geraEstrelas (x-1) (n:l)
                    return (n:ls)




















