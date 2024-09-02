--1
--a
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 =16

--b
g:: Integer -> Integer
g 8=16
g 16=4
g 131=1

--c
h :: Integer -> Integer
h n = f(g n)

k :: Integer -> Integer
k n = g(f n)

--2
--a
absoluto :: Int -> Int
absoluto n | div n 1 <= 0 = n * (-1)
           | otherwise= n

--b
maximoabsoluto :: Int -> Int -> Int
maximoabsoluto n k | absoluto n >= absoluto k = n
                   | otherwise= k

--c
maximo3 :: Int -> Int -> Int -> Int
maximo3 n k j | n > k && n > j && k < j = n
              | k > n && k > j && n < j = k
              | otherwise= j

--d
algunoEs0_1 :: Float -> Float -> Bool
algunoEs0_1 _ 0 = True
algunoEs0_1 0 _ = True
algunoEs0_1 _ _ = False

--e 
ambosSon0 :: Float -> Float -> Bool
ambosSon0 0 0 = True
ambosSon0 _ _ = False

--f
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | (x <= 3 )&& (y <= 3) = True
                   | (3 < x && x <= 7) && (3 < y  && y <= 7) = True
                   | (x > 7) && (y>7) = True
                   | otherwise=False

--g
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | sumaDisAUx x y == False && sumaDisAUx x z == False = 0
                    | sumaDisAUx x z == False && sumaDisAUx x y == True = y
                    | sumaDisAUx y z == False && sumaDisAUx x y == True = x
                    | sumaDisAUx x y == False && sumaDisAUx x z == True = z
                    | otherwise= x + y + z

sumaDisAUx :: Int -> Int -> Bool
sumaDisAUx x y | x == y = False
               | otherwise= True

--h
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod y x == 0 = True
                 | otherwise= False

--i
digitoUnidades :: Int-> Int
digitoUnidades x = mod x 10

--j
digitoDecenas :: Int -> Int
digitoDecenas x = mod (div x 10) 10

--3
estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b = div ((-1)*a) b /= 0 && esMultiploDe b a == True

--4
--a
prodInt :: (Int,Int) -> (Int,Int) -> Int
prodInt (a,b) (c,d)= (a*c )+ (b*d)

--b
todoMenor :: (Int,Int) -> Bool
todoMenor x = fst x < snd x

--c 
distanciaPuntos :: (Float, Float) ->Float
distanciaPuntos (a, b) = sqrt((a*a) + (b*b))

--e
sumarSoloMultiplos :: (Int,Int,Int) -> Int -> Int
sumarSoloMultiplos (a,b,c) n | sumarAux a n && sumarAux b n && sumarAux c n = a+b+c
                             | sumarAux a n == False && sumarAux b n && sumarAux c n = b+c
                             | sumarAux a n == False && sumarAux b n ==False && sumarAux c n = c
                             | sumarAux a n == False && sumarAux b n && sumarAux c n ==False = b
                             | sumarAux a n  && sumarAux b n == False && sumarAux c n ==False = a
                             | sumarAux a n  && sumarAux b n  && sumarAux c n ==False = a+b
                             | sumarAux a n  && sumarAux b n ==False  && sumarAux c n = a+c
                             | otherwise=0

sumarAux :: Int ->Int-> Bool
sumarAux n x | mod n x ==0 =True
             | otherwise= False

sumarSola_1aux :: Int -> Int ->Int
sumarSola_1aux a n|mod a n  == 0 = a
                 | otherwise= 0

                
sumarSoloMultiplos2 :: (Int,Int,Int)-> Int -> Int
sumarSoloMultiplos2 (a,b,c) n = (sumarSola_1aux a n)+ (sumarSola_1aux b n) + (sumarSola_1aux c n)

--f
posPrimerPar :: (Int,Int,Int) -> Int
posPrimerPar (a,b,c) | mod a 2 ==0 = 1
                     | mod b 2==0  = 2 
                     | mod c 2==0  = 3
                     | otherwise=4

--g 
crearPar :: t -> e -> (t,e)
crearPar t e = (t,e)

--h 
invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)

--5
todosMenores :: (Int,Int,Int) -> Bool
todosMenores (a,b,c)= (f1 a > g1 a )&& (f1 b > g1 b) && (f1 c >g1 c)

f1 :: Int -> Int
f1 n | n<=7 = n*n
     | n>7 = 2*n -1

g1:: Int -> Int
g1 n | mod n 2 ==0 = div n 2 
     | otherwise= 3*n +1

--6
type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto n | mod n 4 /= 0= False
           | mod n 100 == 0 && mod n 400 /=0 = False
           | otherwise=True

--7
type Coordenada3d = (Float, Float, Float)

distanciaManhattan :: Coordenada3d -> Coordenada3d -> Float
distanciaManhattan (a,b,c) (d,e,f) = auxiliar2 ((a-d) + (b-e) + (c-f))

auxiliar2 :: Float -> Float
auxiliar2 x |  x>=0 =x
           | x <0= (-1) *x

--8
comparar ::Integer ->Integer ->Integer
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = (-1)
             | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0

sumaUltimosDosDigitos :: Integer ->Integer
sumaUltimosDosDigitos x = mod ((mod (auxiliar3 x) 10) + (div (auxiliar3 x )10)) 10

auxiliar3 :: Integer -> Integer
auxiliar3 x |  x>=0 =x
           | x <0= (-1) *x