fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 =1
fibo n = fibo (n-1) + fibo(n-2)
--2
parteEntera :: Float -> Integer 
parteEntera 0 = 0
parteEntera x | x <=1=0
              | otherwise = 1 +parteEntera (x-1)

--3
esDivisible :: Integer -> Integer -> Bool
esDivisible n h | n ==0 = True
                | n <h =False
                | otherwise= esDivisible (n-h) h 

--4
sumaImpares :: Integer -> Integer 
sumaImpares n| n<1=0
             | otherwise= (2*n-1) + sumaImpares (n-1)

--5

medioFact :: Integer -> Integer 
medioFact 0 = 1
medioFact 1 =1
medioFact n |esPar n = n* medioFact(n-2)
            | esPar n /= True= n* medioFact(n-2)

esPar:: Integer -> Bool
esPar n = mod n 2 ==0


--6
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n<10= True
                      | otherwise= (mod n 10 == mod(div n 10)10) && todosDigitosIguales (div n 10)

--7

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n k = mod (div n (10^(cantDigitos n-  k))) 10

cantDigitos :: Integer -> Integer 
cantDigitos 0=1
cantDigitos n | div n 10 >1 = 1 + cantDigitos (div n 10)
              |n <1 && n >0 = 1

--8
sumaDigitos :: Integer -> Integer 
sumaDigitos n | div n 10 == 0 = n
              | otherwise=mod n 10 + sumaDigitos (div n 10)

--9
esCapicua :: Integer -> Bool
esCapicua n | div n 10 ==0 = True
            | n < 100 && div n 10 /= mod n 10 = False
            | otherwise= esCapicua (div n 10)

--10
f1 :: Integer -> Integer 
f1 n | n == 0 = 1 
     | otherwise= 2^n + f1 (n-1)

f2 :: Integer -> Float -> Float
f2 n q | q==1 = fromInteger n
       | otherwise= q^n + f2 n (q-1)

f3 :: Integer -> Float -> Float 
f3 n q | n==0 = 1
       | n == 1 = fromInteger (2*n)
       | otherwise=f3Aux (2*n) q

f3Aux :: Integer -> Float -> Float
f3Aux 0 q = 0
f3Aux n q = q^n + f3Aux (n -1) q

f4 :: Integer -> Float -> Float
f4 0 q = 1
f4 n q = f4Aux n (2*n) q

f4Aux :: Integer -> Integer -> Float -> Float
f4Aux n j q | n > j =0 
            | otherwise= q^n + f4Aux (n+1) j q

--11
factorial :: Integer -> Integer
factorial 0=1
factorial n = n*factorial (n-1)

eAprox :: Integer -> Float
eAprox n | n ==0=1
         | otherwise=  1/(fromInteger (factorial n)) + eAprox (n-1)


--12
raizDe2Aprox :: Integer -> Float
raizDe2Aprox 1 = 2
raizDe2Aprox n = 2 + (1/(raizDe2Aprox (n-1)))

raizAprox12 :: Integer ->Float
raizAprox12 n = (raizDe2Aprox n)-1
--13
condicion :: Integer -> Integer ->Integer
condicion _ 0=0
condicion x y = x^y + condicion x (y-1)

sumatoria13 :: Integer -> Integer ->Integer
sumatoria13 n 1 = 0
sumatoria13 1 _ = 1
sumatoria13 n m = condicion n m + sumatoria13 (n-1) m

--14
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m  | n ==0 || m==0 = 0
                     | q == 1 = 1
                     | otherwise = sumaPotUno q n m + sumaPotencias q (n-1) m 

sumaPotUno :: Integer -> Integer -> Integer -> Integer
sumaPotUno q _ 0=0
sumaPotUno q 0 _ =0
sumaPotUno q n m = q^(n+m) + sumaPotUno q n (m-1)

--15
sumaRacionales :: Integer ->Integer -> Float
sumaRacionales 0 _ = 0
sumaRacionales _ 0 = 0
sumaRacionales p q = aux15 p q + sumaRacionales p (q-1)

aux15 :: Integer ->Integer -> Float
aux15 0 _ = 0
aux15 p q= (fromInteger p) / (fromInteger q) + aux15 (p-1) q

--16a
menorDivisor :: Integer->Integer
menorDivisor n = menorDivAux n 2

menorDivAux ::Integer->Integer ->Integer
menorDivAux n k | mod n k == 0 = k
                | n == k = n
                | otherwise= menorDivAux n (k+1)

--16b
esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n 

--16c 
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = esPrimo n && esPrimo m && n /= m

--16d
nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoAux 2 n

nEsimoAux :: Integer -> Integer ->Integer
nEsimoAux n q | q==0= n-1
              | esPrimo n = nEsimoAux (n+1) (q-1)
              | otherwise= nEsimoAux (n+1) q

--17 
esFibonacci :: Integer -> Bool
esFibonacci 0 = True
esFibonacci n = esFibonacciAux 0 1 n

esFibonacciAux :: Integer -> Integer -> Integer -> Bool
esFibonacciAux n q m | n ==m=True
                     | n > m =False
                     | otherwise= esFibonacciAux q (n+q) m

--18
mayorDigitoPar ::Integer ->Integer 
mayorDigitoPar n = mayorDigitoParAux n (-1)

mayorDigitoParAux :: Integer ->Integer ->Integer
mayorDigitoParAux 0 m =m
mayorDigitoParAux n m | esPar (mod n 10) &&  (mod n 10) >= m = mayorDigitoParAux (div n 10) (mod n 10)
                      | otherwise= mayorDigitoParAux (div n 10) m
 
--19
esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = esSumaAux n 0 2

esSumaAux :: Integer -> Integer-> Integer -> Bool
esSumaAux n s k | s==n =True
                | s>n=False
                | esPrimo k = esSumaAux n (s+k) (k+1)
                | otherwise= esSumaAux n s (k+1)

--20
tomaValorMax :: Integer ->Integer ->Integer
tomaValorMax n m = tomaValores n m n

tomaValores :: Integer ->Integer ->Integer->Integer
tomaValores n m j | n > m = j 
                  | sumaDeDivisores n > sumaDeDivisores j = tomaValores (n+1) m n
                  | otherwise= tomaValores (n+1) m j

sumaDeDivisores :: Integer ->Integer
sumaDeDivisores n =  sumaDivAUX n n

sumaDivAUX :: Integer ->Integer ->Integer
sumaDivAUX n 0= 0
sumaDivAUX n m | mod n m ==0= m + sumaDivAUX n (m-1)
               | otherwise= sumaDivAUX n (m-1)


--21
pitagoras :: Integer ->Integer ->Integer ->Integer
pitagoras 0 q r = pitagorasAux  0 q r
pitagoras p q r= pitagorasAux p q r + pitagoras (p-1) q r 

pitagorasAux :: Integer ->Integer ->Integer ->Integer
pitagorasAux p 0 r | (p^2>r^2)= 0
                   | (p^2<=r^2) =1
pitagorasAux p q r | (p^2+q^2 <= r^2) =1 + pitagorasAux p (q-1) r
                   | otherwise= pitagorasAux p (q-1) r