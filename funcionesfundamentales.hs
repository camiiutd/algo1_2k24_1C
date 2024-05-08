minimo :: [Int] -> Int
minimo (x:xs) | x <= minimo xs = x
			  | otherwise = minimo xs

maximo :: [Int] -> Int
maximo (x:xs) | x >= maximo xs = x 
              | otherwise = maximo xs

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys) | x == y = ys
                | otherwise = quitar x ys

reverso :: [t] -> [t]
reverso (x:[]) = [x]
reverso (x:xs) = reverso xs++[x]

longitud :: [a] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece y (x:xs) | y == x = True
                   | otherwise = pertenece y xs

maximo :: [Int] -> Int
maximo [] = 0
maximo (x:xs) | head ([x:xs]) >= xs = x
              | otherwise = maximo (tail (x:xs))

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorHasta n 2

menorDivisorHasta :: Int -> Int -> Int 
menorDivisorHasta n q | mod n q == 0 = q
                      | otherwise = menorDivisorHasta n (q+1)

esPrimo :: Int ->Bool
esPrimo n | menorDivisor n == n = True
          | otherwise = False

esPrimoLista :: [Int] -> [Int]
esPrimoLista [] = []
esPrimoLista (x:xs) | esPrimo x = x : esPrimoLista xs
                    | otherwise = esPrimoLista xs