import Data.List

-- 1
data Frac = F Integer Integer
-- a)
normaliza :: Frac -> Frac
normaliza (F a b) = if b < 0 then F (-1*(div a x)) (-1*(div b x))
                                else F (div a x) (div b x)
  where x = mdc (abs a) (abs b)

mdc :: Integer -> Integer -> Integer
mdc x y 
 | x == y = x
 | x < y = mdc x (y-x)
 | x > y = mdc (x-y) y

-- b)
instance Eq Frac where
    f1 == f2 = a1 == a2 && b1 == b2
        where (F a1 b1) = normaliza f1
              (F a2 b2) = normaliza f2

-- c)
instance Ord Frac where
    compare (F a b) (F c d)
       | (a * d) < (b * c)  = LT
       | (a * d) == (b * d) = EQ 
       | (a * d) > (b * d)  = GT 

-- d)
instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b

-- e)
instance Num Frac where
    (F a b) + (F c d) = normaliza (F ((a*d)+(c*b)) (b*d))
    (F a b) * (F c d) = normaliza (F (a*c) (b*d))
    negate (F a b)    = F (-1*a) b
    abs (F a b)       = F (abs a) (abs b)
    signum (F a b)    = F ((signum a)*(signum b)) 1
    fromInteger x     = F x 1

-- f)

-- 2
data Exp a = Const a | Simetrico (Exp a) | Mais (Exp a) (Exp a) | Menos (Exp a) (Exp a) | Mult (Exp a) (Exp a)

-- a)
instance Show a => Show (Exp a) where
      show (Const a)     = show a
      show (Simetrico a) = "-" ++ show a
      show (Mais a b)    = show a ++ "+" ++ show b
      show (Menos a b)   = show a ++ "-" ++ show b
      show (Mult a b)    = show a ++ "*" ++ show b

-- b)
instance (Num a, Eq a) => Eq (Exp a) where
  exp1 == exp2 = calcula exp1 == calcula exp2

calcula :: (Eq a, Num a) => Exp a -> a
calcula (Const a)     = a
calcula (Simetrico a) = -1 * calcula a
calcula (Mais a b)    = calcula a + calcula b
calcula (Menos a b)   = calcula a - calcula b
calcula (Mult a b)    = calcula a * calcula b

-- c)
instance (Eq a,Num a) => Num (Exp a) where
  exp1 + exp2          = Const ((calcula exp1) + (calcula exp2))
  exp1 * exp2          = Const ((calcula exp1) * (calcula exp2))
  negate (Const a)     = Const (-a)
  negate (Simetrico a) = a
  negate (Mais a b)    = Mais (-a) (-b)
  negate (Menos a b)   = Mais (-a) (b)
  negate (Mult a b)    = Mult (-a) b
  abs (Const a)        = Const (abs a)
  abs (Simetrico a)    = abs a
  abs (Mais a b)       = abs (a + b)
  abs (Menos a b)      = abs (a - b)
  abs (Mult a b)       = abs (a * b)
  signum exp           = Const (signum (calcula exp))
  fromInteger x        = Const (fromIntegral x) 

-- 3
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving (Eq)
data Extracto = Ext Float [(Data, String, Movimento)]

extex = Ext 300 [(D 5 4 2010 , "DEPOSITO", Credito 2000),(D 10 8 2010, "COMPRA", Debito 37.5),(D 1 9 2010, "LEV", Debito 60),(D 7 1 2011,"JUROS", Credito 100),(D 22 1 2011, "ANUIDADE", Debito 8)]

-- a)
instance Ord Data where
  (D x1 y1 z1) <= (D x2 y2 z2) = x1 <= x2 && (y1 == y2 && z1 == z2) || y1 < y2 && z1 == z2 || z1 < z2 

-- b)
instance Show Data where
  show (D x y z) = show x ++ "/" ++ show y ++ "/" ++ show z 

-- c)
ordena ::  Extracto -> Extracto
ordena (Ext x l) = Ext x (sortBy (\ (d1,_,_) (d2,_,_) -> compare d1 d2) l )

-- d)
instance Show Extracto where
  show ext = "Saldo anterior : " ++ show v ++ "\n" ++
             "---------------------------------------" ++ "\n" ++
             "Data       Descricao   Credito   Debito" ++ "\n" ++
             "---------------------------------------" ++ "\n" ++ 
             concat (map convertToString l) ++
             "---------------------------------------" ++ "\n" ++ 
             "Saldo Atual : " ++ show (v + saldo (Ext v l))
    where (Ext v l) = ordena ext


convertToString :: (Data, String, Movimento) -> String
convertToString (d,s,Credito x) = show d ++ replicate (11 - length (show d)) ' ' ++ s ++ replicate (12 - length s) ' ' ++ show x ++ "\n"
convertToString (d,s,Debito x) = show d ++ replicate (11 - length (show d)) ' ' ++ s ++ replicate (22 - length s) ' ' ++ show x ++ "\n"

saldo :: Extracto -> Float
saldo (Ext n []) = 0
saldo (Ext n ((d, s, Credito x) : xs)) = x + saldo (Ext n xs)
saldo (Ext n ((d, s, Debito x) : xs))  = -x + saldo (Ext n xs)































