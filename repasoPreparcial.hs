-- ej4 2° tema de recu20241C

esCaminoCollatz :: [Int] -> Int -> Bool
esCaminoCollatz [] _ = True
esCaminoCollatz (x:xs) n | collatz x == collatz n = esCaminoCollatz xs (collatz n) 
                         | otherwise= False

collatz :: Int-> Int
collatz 1=1
collatz n | mod n 2 == 0 = div n 2
          | otherwise= 3*n +1

-- esCaminoCollatz [ 27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242,,723, 1] 27

--PARCIAL DE CODIFICAR--

{--- ejercicio 1
problema hayQueCodificar (c: Char, mapeo: seq(Char x Char)): Bool { 
    requiere: (No hay elementos repetidos entre las primeras componentes de mapeo} 
    requiere: {No hay elementos repetidos entre las segundas componentes de mapeo} 
    asegura: {res = true <=> c es igual a la primera componente de alguna tupla de mapeo} 
-}

hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar c (x:xs) | c == fst x = True
                         | otherwise= hayQueCodificar c xs

--  'c' [('c','n'),('t','y'),('a','b')] 

{-problema cuantasVecesHayQueCodificar (c. Char, frose: seq(Char), mapeo: seq(Char x Char)): Z{
   requiere: (No hay elementos repetidos entre las primeras componentes de mapeo}
   requiere: (No hay elementos repetidos entre las segundas componentes de mapeo}
   requiere: {{frase |>0}
   requiere: (c pertenece a frase)
   asegura: {(res = 0 y hayQueCodificar (c, mapeo) = false) o (res = cantidad de veces que c aparece en frase y hayQueCodificar (c, mapeo) = true)}
-}

cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] ->Int
cuantasVecesHayQueCodificar _ [] _ =0
cuantasVecesHayQueCodificar _ _ [] =0
cuantasVecesHayQueCodificar c frase mapeo | hayQueCodificar c mapeo ==False=0
                                          | otherwise= contador c frase

contador :: Char -> [Char] -> Int
contador _ [] = 0
contador c (x:xs) | c== x = 1+ contador c xs
                  | otherwise= contador c xs 

{--- Ejercicio 3
problema laQueMasHayQueCodificar (frase: seq(Char), mapeo: seq(Char x Char)): Char {
requiere: (No hay elementos repetidos entre las primeras componentes de mapeo)
requiere: (No hay elementos repetidos entre las segundas componentes de mapeo}
requiere: ((frase >0}
requiere: (Existe al menos un c que pertenece a frase y hayQueCodificar(c, mapeo)=true)
asegura: {res = c donde c es el caracter tal que cuantasVecesHayQueCodificar (c, frase, mapeo) es mayor a cualquier otro caracter perteneciente a frase) 
asegura: (Si existen más de un caracter c que cumple la condición anterior, devuelve el que aparece primero en frase}
-}

laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] ->Char
laQueMasHayQueCodificar [f] _ = f
laQueMasHayQueCodificar (f:fs) (x:xs) | cuantasVecesHayQueCodificar f (f:fs) (x:xs) >= maximo fs (x:xs) = f 
                                      | otherwise= laQueMasHayQueCodificar fs (x:xs)

maximo:: [Char] -> [(Char,Char)] ->Int
maximo _ [] = 0
maximo [] _ = 0
maximo (f:fs) (x:xs) | cuantasVecesHayQueCodificar f (f:fs) (x:xs) >= maximo fs (x:xs) = cuantasVecesHayQueCodificar f (f:fs) (x:xs)
                     | otherwise= maximo fs (x:xs)

{--- Ejercicio 4 
problema codificarFrase (frase: seq(Char), mapeo: seq(Char x Char)): seq (Char) (
requiere: (No hay elementos repetidos entre las primeras componentes de mapeo) 
requiere: (No hay elementos repetidos entre las segundas componentes de mapeo)
requiere: (frase | >0}
asegura: (res) = | frase|}
asegura: (Para todo 0 <= i < |frase| si hayQueCodificar(frase[i], mapeo) = true entonces res[i]= mapeo[[i]]1. para un i tal que 0 <= i < |mapeo| y mapeo[i]0 = frase [i]
asegura: (Para todo 0 <= i < |frase| si hayQueCodificar(frase[l], mapeo) = false entonces res[i]= frase[i])

-}

codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase _ [] = []
codificarFrase [] _ = []
codificarFrase (f:fs) (x:xs)| hayQueCodificar f (x:xs) = codifiquemos f (x:xs) : codificarFrase fs (x:xs)
                            | otherwise= f : codificarFrase fs (x:xs)

codifiquemos :: Char -> [(Char,Char)] -> Char
codifiquemos c (x:xs) | c== fst x= snd x 
                      | otherwise= codifiquemos c xs

--ELIMINO ELEMENTOS--

eliminorepetidos :: (Eq t) => t -> [t] -> [t]
eliminorepetidos _ [] = []
eliminorepetidos n (x:xs) | n == x = eliminorepetidos n xs 
                          | otherwise= x: eliminorepetidos n xs 

eliminoTodosLosQueRepiten :: (Eq t) => [t] -> [t]
eliminoTodosLosQueRepiten [] = []
eliminoTodosLosQueRepiten (x:xs) | pertenece x xs = x: eliminoTodosLosQueRepiten (eliminorepetidos x xs)
                                 | otherwise= x: eliminoTodosLosQueRepiten xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] =False
pertenece n (x:xs) | n == x = True
                   | otherwise= pertenece n xs 

--REVERSE--
longitud :: (Eq t) => [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 +longitud xs 

ultimo :: (Eq t) => [t] -> t
ultimo [x] =x
ultimo x | longitud x >= 1 = ultimo (tail x)

reverso :: (Eq t) => [t]->[t]
reverso [] = []
reverso [x] = [x]
reverso xs = ultimo xs : reverso (sacoLosUltimos xs)

sacoLosUltimos :: (Eq t) => [t] -> [t]
sacoLosUltimos [] = []
sacoLosUltimos [_] = []  
sacoLosUltimos (x:xs) = x : sacoLosUltimos xs

--

ejParcial :: String  -> Bool
ejParcial [] =  False
ejParcial (x:xs) | x == ' ' = ejParcial xs
                 | ordenado x xs = True
                 | otherwise= False

ordenado :: Char -> String -> Bool
ordenado _ []  = True
ordenado n (x:xs) | x==' '= ordenado n xs
                  | n <= x= ordenado n xs
                  | otherwise= False


esCapicua :: (Eq t) => [t] -> Bool
esCapicua [] = True

sacoUltimo :: (Eq t) => [t] -> t
sacoUltimo [x] = x
sacoUltimo (x:xs) | long (x: xs) >=1 = sacoUltimo xs

long :: (Eq t) => [t] -> Int
long [] = 0
long (x:xs) = 1 + long xs 

reverseee :: (Eq t) => [t] -> [t]
reverseee [] = []
reverseee (x:xs) = sacoUltimo (x:xs) : reverseee (sacoUltimos (x:xs))

sacoUltimos :: (Eq t) => [t] -> [t]
sacoUltimos [] = [] 
sacoUltimos [_] = []
sacoUltimos (x:xs) = x : sacoLosUltimos xs 