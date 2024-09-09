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
iesimoDigito n k = mod (div n 10^((cantDigitos (fromInteger n)-k)) 10)

cantDigitos :: Float -> Integer 
cantDigitos 0=1
cantDigitos n | n/10 >1 = 1 + cantDigitos (n/10)
              | n <1 && n >0 = 1 + cantDigitos n
              | otherwise= 1
