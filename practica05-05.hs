{-Ejercicio 21. Especificar e implementar una funci´on pitagoras :: Integer ->Integer ->Integer ->Integer que dados
m, n , r ∈ N≥0, cuente cu´antos pares (p, q) con 0 ≤ p ≤ m y 0 ≤ q ≤ n satisfacen que p
2 + q
2 ≤ r
2
. Por ejemplo:
pitagoras 3 4 5 ⇝ 20
pitagoras 3 4 2 ⇝ 6-}

esMenorPitagoriano :: Integer->Integer->Integer->Bool
esMenorPitagoriano p q r = p^2 + q^2 <= r^2

pitagorasNFijo :: Integer -> Integer -> Integer -> Integer
pitagorasNFijo n m r | m < 0 = 0
                     | esMenorPitagoriano n m r = 1 + pitagorasNFijo n (m-1) r
                     | otherwise = 0 + pitagorasNFijo n (m-1) r


pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras n m r | n==0 = pitagorasNFijo 0 m r
                | otherwise = pitagorasNFijo n m r + pitagoras (n-1) m r

{-Ejercicio 20. Especificar e implementar la funci´on
 tomaValorMax :: Int ->Int ->Int que dado un n´umero entero n1 ≥ 1
y un n2 ≥ n1 devuelve alg´un m entre n1 y n2 tal que 
sumaDivisores(m) = m´ax{sumaDivisores(i) | n1 ≤ i ≤ n2}-}

{-nCondicion :: Int -> Int -> Bool
nCondicion n1 n2 = n2 >= n1 

sonDivisoresCond :: Int -> Int -> Bool
sonDivisoresCond x y = mod x y == 0

sonDivisores :: Int -> Int -> Int
sonDivisores 0 0 = 0
sonDivisores x y | mod x y == 0 = div x y + sonDivisores x y
                 | otherwise = sonDivisores (x-1) (y-1)
sumaDivisores :: Int -> Int -> Int
sumaDivisores 0 0 = 0
sumaDivisores x y | nCondicion x y == True && sonDivisoresCond x y = sonDivisores x y
                  | otherwise = sumaDivisores (x - 1) (y - 1)

tomaValorMax :: Int -> Int -> Int
tomaValorMax n m  = sumaDivisores n m-}

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n 1 = 1
sumaDivisoresHasta n i | mod n 1 == 0 = i + sumaDivisoresHasta n (i-1)
                       | otherwise = sumaDivisoresHasta n (i-1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

tomaValorMax :: Integer -> Integer
tomaValorMax 1 = 1
tomaValorMax n = max  (sumaDivisores n) (tomaValorMax (n-1))

valorMax :: Integer -> Integer -> Integer
valorMax n1 n2 | n1/=n2 = max (sumaDivisores n1) (valorMax (n1+1) n2)
               | otherwise = sumaDivisores n1 
