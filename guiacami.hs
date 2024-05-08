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

----------
factorial :: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)

----------
cantDigitos :: Int -> Int
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (div n 10)

iesimoDigito :: Int -> Int -> Int
iesimoDigito n i = (abs n `div` (10^(cantDigitos n - i))) `mod` 10

---------
expo :: Int -> Int
expo n = 10^(cantDigitos n - 1)

esCapicua :: Int -> Bool
esCapicua n = n == invertirNumero n

invertirNumero :: Int -> Int
invertirNumero n = read (reverse (show n)) :: Int

----
sumatoria1 :: Int -> Int -> Int
sumatoria1 n i | n < 0 = i
              | otherwise = sumatoria1 (n - 1) (i + 2^n)

f1 :: Int -> Int
f1 n = sumatoria1 n 0
----
sumatoria2 :: Int -> Int -> Int
sumatoria2 n i | n < 0 = i
               | otherwise = sumatoria2 (n-1) (i + n^i)

f2 :: Int -> Float -> Float
f2 n i = sumatoria2 n i 0

------
sumatoria3 :: Int - Int -> Int
sumatoria3 n i | n < 0 = i
              | otherwise = sumatoria3 (n - 1) (i + 2*n^i)

f3 :: Int -> Float -> Float
f3 n i = sumatoria2 n i 0