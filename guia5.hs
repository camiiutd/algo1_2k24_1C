--------1.1
longitud :: [t] -> Int
longitud (x:xs) = length [x:xs]
--------1.2 
ultimo :: [t] -> t
ultimo (x:[]) = x
ultimo (x:xs) = ultimo xs
--------1.3
principio :: [t] -> [t]
principio (x:[]) = []
principio (x:xs) = x : principio xs
--------1.4
reverso :: [t] -> [t]
reverso (x:[]) = [x]
reverso (x:xs) = reverso xs++[x]
-------2.1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True  
                   | otherwise = pertenece x ys
-------2.2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = x == head xs && todosIguales xs
--------2.3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = False
todosDistintos (x:xs) | pertenece x xs = False
                      | otherwise = todosDistintos xs
--------2.4 
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos (tail(x:xs)) --- podes poner xs solo
---------2.5
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys) | x == y = ys
                | otherwise = quitar x ys ---------para no perder el elemento revisado
--------2.6
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y:ys) | x == y = quitarTodos x ys
                     | otherwise = y : quitarTodos x ys 
---------2.7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (y:ys) = y:eliminarRepetidos (quitarTodos y ys)
---------2.8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos _ [] = False
mismosElementos [] _ = False
mismosElementos [] [] = True
mismosElementos (x:xs) (y:ys) | x == y = mismosElementos xs ys 
                              | otherwise = False

---------2.9
capicua :: (Eq t) => [t] -> Bool
capicua [] = True
capicua xs = xs == reverso xs
----------3.1
sumatoria :: (Num t) => [t] -> t
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
---------3.2
productoria :: (Num t) => [t] -> t
productoria [] = 1
productoria (x:xs) = x * productoria xs
---------3.3
maximo :: [Int] -> Int
maximo [] = 0
maximo (x:xs) | head ([x:xs]) >= xs = x
              | otherwise = maximo (tail (x:xs))
---------3.4
sumarN :: Int -> [Int] -> [Int]
sumarN x (y:ys) | x == 0 = (y:ys)
                | x == 1 = (x+y:ys)
                | otherwise = sumarN x (tail(y:ys))
--------3.5
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = x + x : xs
---------3.6
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarN (ultimo xs) xs
-----------3.7
pares2 :: [Int] -> [Int] 
pares2 [] = []
pares2 (x:xs) | mod x 2 == 0 = x : pares2 xs
              | otherwise = pares2 xs
---------3.8
multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN _ [] = []
multiplosDeN n (x:xs)
    | mod x n == 0 = x : multiplosDeN n xs
    | otherwise = multiplosDeN n xs

--------3.9
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar xs = (ordenar(quitar(maximo xs)xs))++[maximo xs]
----------4.a
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos (x:xs) | x == ' ' = xs
                             | otherwise = (x:xs)

----------4.b


contarPalabras :: [[Char]] -> Integer
contarPalabras [] = 0
contarPalabras (x:xs)
    | esPalabra x = 1 + contarPalabras xs  
    | otherwise = contarPalabras xs        

esPalabra :: [Char] -> Bool
esPalabra [] = False
esPalabra (x:xs) = not (elem ' ' (x:xs)) 

------------ 4.c
palabras :: [Char] -> [[Char]]
palabras y = palabrasAux y []

palabrasAux :: [Char] -> [Char] -> [[Char]]
palabrasAux [] y = [y]
palabrasAux (x:xs) y | x /= ' ' = (palabrasAux xs (y ++ [x]))
                     | x == ' ' = [y] ++ (palabrasAux xs []) 

-----------4.e
aplanar ::  [[Char]] -> [Char]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs
-----------4.f
aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos [] = []
aplanarConBlancos [x] = x
aplanarConBlancos (x:xs) = x ++ " " ++ aplanarConBlancos xs

------------4.g
aplanarConNBlancos :: [[Char]] -> Integer -> [Char]
aplanarConNBlancos [] _ = " "
aplanarConNBlancos [x] _ = x
aplanarConNBlancos (x:xs) n = x ++ printNBlancos n ++ aplanarConNBlancos xs n

printNBlancos :: Integer -> [Char]
printNBlancos 1 = [' ']
printNBlancos n = ' ' : printNBlancos (n-1)
