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

--4
type Texto = [Char]
--a
sacarBlancosRepetidos :: Texto->Texto
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs) | x == ' ' && y ==' ' = sacarBlancosRepetidos (y:xs)
                               | otherwise= x: sacarBlancosRepetidos (y:xs)

--b
contarPalabras :: Texto -> Integer 
contarPalabras [] =0
contarPalabras [x] =1
contarPalabras (x:xs) | x == ' ' = 1 + contarPalabras xs
                      | otherwise= contarPalabras xs 

--c cortesía d chatgpt
palabras :: Texto -> [Texto]
palabras [] = []
palabras (x:xs) = auxPalabras (x:xs) []

auxPalabras :: Texto->Texto -> [Texto]
auxPalabras [] [] = []
auxPalabras [] n = [n]
auxPalabras (x:xs) n |x ==' ' && n ==[] = auxPalabras xs n
                     | x== ' ' = n : auxPalabras xs []
                     | otherwise= auxPalabras xs (n ++ [x])
                    
--d
palabraMasLarga :: Texto-> Texto
palabraMasLarga [] =[]
palabraMasLarga [x] =[x]
palabraMasLarga (x:xs) = maximoPalabras (palabras (x:xs))

maximoPalabras :: [Texto] -> Texto
maximoPalabras [] =[]
maximoPalabras (x:xs) | longitud x > longitud (maximoPalabras xs) = x
                      | otherwise= maximoPalabras xs


--e
aplanar :: [Texto] ->Texto
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

--f
aplanarConBlancos ::[Texto] -> Texto
aplanarConBlancos [] = []
aplanarConBlancos (x:xs) = x ++ ' ' : aplanarConBlancos xs

--g
aplanarConNBlancos :: [Texto] -> Integer -> Texto
aplanarConNBlancos [] _ = []
aplanarConNBlancos [x] _ = x
aplanarConNBlancos (x:xs) n = x ++ multiplicoEspacios n ++ aplanarConNBlancos xs n

multiplicoEspacios :: Integer -> Texto
multiplicoEspacios 0 = []
multiplicoEspacios n =' ' : multiplicoEspacios (n-1)

--5.1
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada (x:xs) = sumaAcuAux xs x [x]

sumaAcuAux :: (Num t) => [t] -> t -> [t] -> [t]
sumaAcuAux [] _ [] = []
sumaAcuAux [] _ ns = reverso ns
sumaAcuAux (x:xs) y ns =sumaAcuAux xs (y+x) ((y+x ):ns)

--5.2
descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos xs = descomponerPrimosAux xs

descomponerPrimosAux :: [Integer] ->[[Integer]]
descomponerPrimosAux []= []
descomponerPrimosAux (x:xs) = divisoresDelNum x 2 : descomponerPrimosAux xs 

divisoresDelNum :: Integer -> Integer -> [Integer]
divisoresDelNum x n | n > x= []
                    | mod x n ==0 && esPrimo n = n : divisoresDelNum x (n+1) 
                    | otherwise= divisoresDelNum x (n+1)
                    
menorDivisor :: Integer->Integer
menorDivisor n = menorDivAux n 2

menorDivAux ::Integer->Integer ->Integer
menorDivAux n k | mod n k == 0 = k
                | n == k = n
                | otherwise= menorDivAux n (k+1)

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n 
--6a

type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

enLosContactos :: Nombre -> ContactosTel ->Bool
enLosContactos n c = enLosContactosAux n c

enLosContactosAux :: [Char] -> [([Char], [Char])] ->Bool
enLosContactosAux _ [] = False
enLosContactosAux n ((a,b):ys) | n == a = True
                               | otherwise= enLosContactosAux n ys

--6b
agregarContacto :: Contacto ->ContactosTel ->ContactosTel
agregarContacto c ct = agregarContactoAux c ct

agregarContactoAux :: ([Char], [Char]) -> [([Char], [Char])] -> [([Char], [Char])]
agregarContactoAux _ [] = []
agregarContactoAux (a,b) tel | enLosContactos a tel == True =  (a,b) : eliminoTupla (a,b) tel
                             | otherwise= (a,b) : tel

eliminoTupla :: ([Char], [Char]) -> [([Char], [Char])] -> [([Char], [Char])]
eliminoTupla _ [] =[]
eliminoTupla (a,b) ((c,d):ys) | a == c = ys
                              | otherwise= (c,d) : eliminoTupla (a,b) ys

--6c
--(lo hice arriba mas o menos es lo mismo :vvvvvvvvvvvvvvvvv)
eliminarContacto :: [Char] -> [([Char], [Char])] -> [([Char], [Char])]
eliminarContacto _ [] = []
eliminarContacto n ((a,b):xs) | a == n = xs 
                              | otherwise= (a,b) : eliminarContacto n xs

--7
type Identificacion = Integer
type Ubicacion = Texto --STRING 
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool

existeElLocker :: Integer ->[(Integer, (Bool, [Char]))] ->Bool
existeElLocker n ((a,b):xs) | n == a = True
                            | otherwise= existeElLocker n xs 

--7.2
ubicacionDelLocker :: Integer -> [(Integer, (Bool, [Char]))] ->[Char]
ubicacionDelLocker _ [] = []
ubicacionDelLocker n ((a,b):xs) | n == a = (snd b )
                                | otherwise= ubicacionDelLocker n xs

--7.3
estaDisponibleElLocker :: Integer -> [(Integer, (Bool, [Char]))] ->Bool
estaDisponibleElLocker n ((a,b):xs) | n == a = (fst b)
                                    | otherwise= estaDisponibleElLocker n xs

--7.4  
ocuparLocker :: Integer -> [(Integer, (Bool, [Char]))]->[(Integer, (Bool, [Char]))]
ocuparLocker n ((a,(b,c)):xs) |n==a && b ==False = (a,(True,c)) : xs
                              | n==a&& b==True=((a,(b,c)):xs)
                              | otherwise= (a,(b,c)) :ocuparLocker n xs


eliminaLocker :: Integer -> [(Integer, (Bool, [Char]))]->[(Integer, (Bool, [Char]))] 
eliminaLocker _ [] =[]
eliminaLocker n ((a,b):xs) | n ==a = xs
                           | otherwise= (a,b) : eliminaLocker n xs 


