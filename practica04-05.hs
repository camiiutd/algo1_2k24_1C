{-Ejercicio 5. Definir las siguientes funciones sobre listas:
1. sumaAcumulada :: (Num t) => [t] -> [t] seg´un la siguiente especificaci´on:
problema sumaAcumulada (s: seq⟨T⟩) : seq⟨T⟩ {
requiere: {T es un tipo num´erico}
requiere: {cada elemento de s es mayor estricto que cero}
asegura: {|s| = |resultado| ∧ el valor en la posici´on i de resultado es Pi
k=0 s[k]}
}
Por ejemplo sumaAcumulada [1, 2, 3, 4, 5] es [1, 3, 6, 10, 15].
-}

suma :: (Num t) => [t] -> t
suma [] = 0
suma (x:xs) = head (x:xs) + suma xs


sumaAcumulada :: (Eq t , Num t) => [t] -> [t]
sumaAcumulada [x] = [x]
sumaAcumulada (x:xs) = head (x:xs) : sumaAux x xs

sumaAux :: (Num t) => t -> [t] -> [t]
sumaAux _ [] = []
sumaAux y (x:xs) = (y + x) : sumaAux (y + x) xs

{-2. descomponerEnPrimos :: [Integer] -> [[Integer]] seg´un la siguiente especificaci´on:
problema descomponerEnPrimos (s: seq⟨Z⟩) : seq⟨seq⟨Z⟩⟩ {
requiere: { Todos los elementos de s son mayores a 2 }
asegura: { |resultado| = |s| }
asegura: {todos los valores en las listas de resultado son n´umeros primos}
asegura: {multiplicar todos los elementos en la lista en la posici´on i de resultado es igual al valor en la posici´on
i de s}
}
Por ejemplo descomponerEnPrimos [2, 10, 6] es [[2], [2, 5], [2, 3]].-}

{-descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) | x /= esPrimo (x:xs) = head (x:xs) : []
                           | otherwise = descomponerEnPrimos xs 

esPrimo :: Int -> Bool
esPrimo x | x <= 1 = False
               | otherwise = esDivisible x (x-1)

esDivisible :: Int -> Int -> Bool
esDivisible x 1 = True
esDivisible x y = mod x y == 0
-}


descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = primosQueDividen x 2 : descomponerEnPrimos xs

primosQueDividen :: Integer -> Integer-> [Integer]
primosQueDividen n q | n == 0 || n == 1 = []
                     | esPrimo n = [n]
                     | mod n (proxPrimo q) == 0 = q : primosQueDividen (div n q) (proxPrimo q)
                     | otherwise = primosQueDividen n ((proxPrimo q) + 1)

proxPrimo :: Integer -> Integer
proxPrimo p | esPrimo p = p
            | otherwise = proxPrimo (p+1)

menorDivisor :: Integer ->Integer
menorDivisor n = menorDivisorHasta n 2

menorDivisorHasta :: Integer -> Integer -> Integer -- se requiere que q == 2
menorDivisorHasta n q | mod n q == 0 = q
                      | otherwise = menorDivisorHasta n (q+1)

esPrimo :: Integer ->Bool
esPrimo n | menorDivisor n == n = True
          | otherwise = False
-----------

{- sumarElUltimo :: [Integer] -> [Integer] seg´un la siguiente especificaci´on:
problema sumarElUltimo (s: seq⟨Z⟩) : seq⟨Z⟩ {
requiere: { |s| > 0 }
asegura: {resultado = sumarN(s[|s| − 1], s) }
}
Por ejemplo sumarElUltimo [1,2,3] da [4,5,6]
-}

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo 