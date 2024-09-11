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
       | otherwise= q^(2*n) + f3 (n-1) q