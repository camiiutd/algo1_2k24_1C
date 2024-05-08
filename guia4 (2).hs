factorial :: Int -> Int
factorial n | n == 0 = 1
            | otherwise =  n* factorial (n-1)

eAprox :: Int -> Float
eAprox n | n == 0 = 1
         | n == 1 = 1
         | otherwise = eAprox (n-1) + (1/fromIntegral (factorial n))

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
sonCoprimos n m | menorDivisor n == menorDivisor m = True
                | otherwise = False

-- Función para calcular el n-ésimo primo
nEsimoPrimo :: Int -> Int 
nEsimoPrimo n
  | n == 1    = 2
  | otherwise = nEsimoPrimoDesde 3 2  -- Empezamos desde 3, ya que 2 es el primer primo
  where
    nEsimoPrimoDesde :: Int -> Int -> Int
    nEsimoPrimoDesde numero contador
      | contador == n = numero - 1 -- Restamos 1 para compensar el exceso en el contador
      | esPrimo numero = nEsimoPrimoDesde (numero + 2) (contador + 1) -- Saltamos los números pares
      | otherwise = nEsimoPrimoDesde (numero + 2) contador -- Saltamos los números pares

