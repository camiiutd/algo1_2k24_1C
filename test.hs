f :: Int -> Int
f 1 = 8
f 4 = 131
f 16 = 16
-----------
g :: Int -> Int
g 8 = 16
g 16 = 4
g 131 = 1
-----------
h :: Int -> Int
h x = f (g x)
-------------
absoluto :: Int -> Int
absoluto x = abs x
----------
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | x >= y = x
                   | otherwise = y
-----------------
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | x == y && x == z && y == z = 0
              | x /= y && x == z && y == z = x 
              | x == y = z
              | x /= y && x /= z && y /= z = x + y + z
              | otherwise = y
-----------------
algunoES0 :: Int -> Int -> Bool
algunoES0 x y | x == 0 = True
              | otherwise = False
----------------
ambosSon0 :: Int -> Int -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False
----------------

mismoIntervalo :: Float -> Float -> String
mismoIntervalo x y
    | x <= 3 && y <= 3 = "Pertenecen al intervalo <= 3"
    | x > 3 && x <= 7 && y > 3 && y <= 7 = "Pertenecen al intervalo 3 < x <= 7"
    | x > 7 && y > 7 = "Pertenecen al intervalo > 7"
    | otherwise = "No están relacionados"



----------------
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x >= y && x >= z = x
              | y >= z && y >= x = y
              | otherwise = z
---------------
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = x `mod` y == 0
---------------
digitoUnidades :: Int -> Int
digitoUnidades n = abs n `mod` 10
--------------
digitoDecenas :: Int -> Int
digitoDecenas n = abs n `mod` 100
--------------
---estanRelacionados :: Int -> Int -> Bool
---estanRelacionados x y = x /= 0 && y /= 0 && x * x + x * y
-----------
prodint2 :: (Float, Float) -> (Float, Float) -> Float
prodint2 (a, b) (c, d) = (a * c) + (b * d) 

-------
todoMenor2 :: (Float, Float) -> (Float, Float) -> Bool
todoMenor2 (a, b) (c, d) = c >= a && d >= b
-------

distanciaPuntos2 :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos2 (a, b) (c, d) = sqrt((a - c)^2 + (b - d)^2)
-----

sumaTerna1 :: (Int, Int, Int) -> Int
sumaTerna1 (a, b, c) = (a + b + c)
------
sumarSoloMultiplos1 :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos1 (a,b,c) x | (mod a x == 0 && mod b x == 0 && mod c x == 0) = a + b + c
                              | (mod a x /= 0 && mod b x /= 0 && mod c x == 0) = c
                              | (mod a x /= 0 && mod b x == 0 && mod c x /= 0) = b
                              | (mod a x == 0 && mod b x /= 0 && mod c x /= 0) = a
                              | (mod a x == 0 && mod b x == 0 && mod c x /= 0) = a + b
                              | (mod a x /= 0 && mod b x == 0 && mod c x == 0) = b + c
                              | (mod a x == 0 && mod b x /= 0 && mod c x == 0) = a + c
                              | otherwise = 0

------
posPrimerPar1 :: (Int, Int, Int) -> Int
posPrimerPar1 (a,b,c) | mod a 2 == 0 = 1
                      | mod b 2 == 0 = 2
                      | mod c 2 == 0 = 3
                      | otherwise = 4

---------

crearPar2 :: a -> b -> (a,b)
crearPar2 x y = (x, y)

--------
invertir2 ::(a,b) -> (b,a)
invertir2 (a,b) = (b,a)

--------
f1 :: Int -> Int
f1 x | 7 >= x = x^2 
     | x > 7 = 2*x - 1

g1 :: Int -> Int
g1 x | mod x 2 == 0 = div x 2 
     | otherwise = 3*x + 1


todoMenor222 :: (Int, Int, Int) -> Bool
todoMenor222 (a,b,c) = (f1 (a)) > (g1 (a)) && (f1 (b)) > (g1 (b)) && (f1(c)) > (g1(c))

----------

bisiesto1 :: Int -> Bool
bisiesto1 x | mod x 4 /= 0 = False
            | mod x 100 == 0 && mod x 400 /= 0 = False
            | otherwise = True

-----------
distanciaManhattan1 :: (Float, Float, Float) ->(Float, Float, Float) ->Float
distanciaManhattan1 (x,y,z) (a,b,c) = abs ((x - a) + (y - b) + (z - c))

------------
sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x = (mod (abs x) 10) + (mod (div (abs x) 10) 10)

comparar :: Int -> Int -> Int
comparar x y | sumaUltimosDosDigitos (x) < sumaUltimosDosDigitos (y) = 1
             | sumaUltimosDosDigitos (x) > sumaUltimosDosDigitos (y) = (-1)
             | sumaUltimosDosDigitos (x) == sumaUltimosDosDigitos (y) = 0

--------
