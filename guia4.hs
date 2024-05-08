fibonacci :: Int -> Int 
fibonacci x | x == 0 = 0
            | x == 1 = 1
            | otherwise = fibonacci (x-1) + fibonacci (x-2)
---------2
parteEntera :: Float -> Int
parteEntera x = floor x

--------- 3
esDivisible :: Int -> Int -> Bool
esDivisible x y
    | y == 0    = False
    | x < y     = False
    | x == y    = True
    | otherwise = esDivisible (x - y) y

------------- 4
sumaImpares :: Int -> Int
sumaImpares n | 0 > n = 0
              | otherwise = sum [2 * i - 1 | i <- [1..n]]
-------------5
medioFact :: Int -> Int
medioFact n | n == 0 || n == 1 = 1
            | otherwise = n * medioFact (n-2)
-----------6
sumaDigitos :: Int -> Int
sumaDigitos n | n < 10 = n
              | otherwise = rem n 10 + sumaDigitos (div n 10)

----------7
todosDigitosIguales :: Int -> Bool
todosDigitosIguales n | n < 0 = False
                      | n < 100 = rem n 10 == div n 10
                      | otherwise = todosDigitosIguales (rem n 100) && todosDigitosIguales (div n 10)

--------
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
f2 n i = fromIntegral (sumatoria2 n (round i))

---------

sumatoria3 :: Int -> Int -> Int
sumatoria3 n i | n < 0 = i
               | otherwise = sumatoria3 (n-1) (i + n^2*i)

f3 :: Int -> Int -> Float
f3 n i = fromIntegral (sumatoria3 n i)

--------

f4 :: Int -> Float -> Float
f4 n q = f2 (2*n) q - f2 (n-1) q

-------11
eAprox :: Int -> Float
eAprox n = fromIntegral (funx (n - 1))

funx :: Int -> Int
funx n = div 1 (factorial n)

------12

sucesion :: Int -> Float
sucesion 1 = 2
sucesion n = 2 + 1 / (fromIntegral n + sucesion (n-1))

raizDe2Aprox :: Int -> Float
raizDe2Aprox n | n > 1 = sucesion (n-1)
               | otherwise = 1

----------13

fun13 :: Int -> Int -> Int
fun13 0 m = 0
fun13 n m = fun13 (n-1) m + sumatoria13 n m


sumatoria13 :: Int -> Int -> Int
sumatoria13 n m = n^m + sumatoria13 (n-1) m

-----------14
sumaPotencias :: Int -> Int -> Int -> Int 
sumaPotencias q n m | q == 0 = 0
                    | otherwise = q^(potenciasn n + potenciasn m)


potenciasn :: Int -> Int
potenciasn n | n == 1 = 1 
             | otherwise = n * (n-1)

-----------15

sumaRacionales :: Int -> Int -> Float 
sumaRacionales n m | n == 1 && m == 1 = 1 
                   | otherwise = (sumatoria15 n)/(sumatoria15 m)

sumatoria15 :: Int -> Float
sumatoria15 p | p == 1  = 1 
              | otherwise = fromIntegral (p * (p-1))

-----------16 a
primo :: Int -> Int -> Int
primo k p | mod k p == 0 = p
          | otherwise = primo k (p+1)

menorDivisor :: Int -> Int
menorDivisor n | n == 1 = 1
               | otherwise = primo n 2


-----------16 b
esPrimo :: Int -> Bool 
esPrimo n | n == menorDivisor n = True
          | otherwise = False

-----------16 c

sonCoprimos :: Int -> Int -> Bool
sonCoprimos n m | menorDivisor n /= menorDivisor m = True
                | otherwise = False

---------16 d
nEsimoPrimo :: Int -> Int 
nEsimoPrimo n | n == 1 = 2
              | otherwise = nesimocuenta (nEsimoPrimo (n-1))

nesimocuenta :: Int -> Int
nesimocuenta n | esPrimo (n+1) = n+1
               | otherwise = nesimocuenta (n+1)

----------17
esFibonacci

esFibonacci :: Int -> Bool
esFibonacci n = esFibonacci nesimocuenta (n-1)