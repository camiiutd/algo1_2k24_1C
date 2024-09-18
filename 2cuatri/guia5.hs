--1 
longitud :: [t]-> Integer
longitud [] =0
longitud (x:xs) = 1 + longitud xs

--2
ultimo :: [t]->t
ultimo [x] =x
ultimo xs | longitud xs >1 = ultimo (tail xs )

--3
principio :: [t] -> [t]
principio [] =[]
principio xs=xs

--4
reverso :: [t]->[t]
reverso [] = []
reverso [x] = [x]
reverso xs = ultimo xs : reverso (sacoUltimo xs)

sacoUltimo ::  [t]->[t]
sacoUltimo [] =[]
sacoUltimo (x:xs) | longitud xs ==0=[]
                  | longitud xs ==1=[x]
                  | otherwise= [x] ++ sacoUltimo xs

--2
--1

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False  
pertenece n (x:xs) | n ==x = True
                   | otherwise= pertenece n xs

--2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs)| pertenece x xs = todosIguales xs
                   | otherwise= False

--3
todosDistintos :: (Eq t) => [t] ->Bool
todosDistintos [] = True
todosDistintos [x] =True
todosDistintos (x:xs)| pertenece x xs = False
                     | otherwise=todosDistintos xs 

--4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = True
hayRepetidos [x] = False
hayRepetidos (x:xs) | pertenece x xs == True = True
                    | otherwise= hayRepetidos xs
--5
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar m (x:xs) | pertenece m (x:xs) == False = (x:xs)
                | m==x = xs
                | otherwise= x: quitar m xs
--6
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos m (x:xs) |pertenece m (x:xs) == False = (x:xs)
quitarTodos m (x:xs) | m/=x = x : quitarTodos m xs
                     | otherwise= quitarTodos m xs

--7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) | pertenece x xs = x: eliminarRepetidos( quitarTodos x xs )
                         | otherwise= x : eliminarRepetidos xs                         

--8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos _ [] =True
mismosElementos [] _ = True
mismosElementos (x:xs) ys | pertenece (head (eliminarRepetidos (x:xs))) (eliminarRepetidos ys)= mismosElementos xs ys
                          | not (pertenece (head (eliminarRepetidos (x:xs))) (eliminarRepetidos ys)) = False 
                          | otherwise= True 

--9
capicua :: (Eq t) => [t] ->Bool
capicua [] = True
capicua [x] = True
capicua xs = xs == (reverso xs)

--3
--1
sumatoria :: [Integer] -> Integer 
sumatoria [] =0
sumatoria (x:xs) = x + sumatoria xs

--2
productoria :: [Integer] -> Integer
productoria [] =1
productoria (x:xs) = x * productoria xs

--3
maximo :: [Integer] -> Integer
maximo [] = 0
maximo [x] =x
maximo (x:xs) | x >= maximo xs = x
              | otherwise=maximo xs

--4
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (n+x) : sumarN n xs

--5
sumarElPrimero :: [Integer] -> [Integer] 
sumarElPrimero [] =[]
sumarElPrimero (x:xs) = (x+x) : sumarN x xs

--6
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo (x:xs) = (ultimo (x:xs) + x) : sumarElUltimo xs  

--7
pares :: [Integer] ->[Integer] 
pares [] = []
pares (x:xs) | mod x 2 ==0= x : pares xs
             | otherwise= pares xs

--8 
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | mod x n ==0= x : multiplosDeN n xs
                      | otherwise= multiplosDeN n xs

--9
ordenar :: [Integer] -> [Integer] 
ordenar [] = []
ordenar xs = maximo xs : ordenar (quitar (maximo xs) xs)