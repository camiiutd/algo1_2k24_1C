import Data.Char

--divisores parcial

divisoresPropios :: Int -> [Int]
divisoresPropios n = divisoresAux n 1

divisoresAux :: Int -> Int -> [Int]
divisoresAux n a | a >= n = []
                 | mod n a == 0 = a : divisoresAux n (a+1)
                 | otherwise = divisoresAux n (a+1)

--2
sonAmigos :: Int -> Int -> Bool
sonAmigos n k = sumaLista (divisoresPropios n) == k && sumaLista (divisoresPropios k) == n

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs)= x + sumaLista xs

--3
losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos n = nPerfectosAux n 1

nPerfectosAux :: Int -> Int -> [Int]
nPerfectosAux 0 _ = []
nPerfectosAux n k  | esPerfecto k (divisoresPropios k) = k : nPerfectosAux (n-1) (k+1)
                   | otherwise= nPerfectosAux n (k+1)

esPerfecto :: Int -> [Int] -> Bool
esPerfecto n divisores = n == sumaLista divisores

--4
listaDeAmigos :: [Int] -> [(Int,Int)]
listaDeAmigos [] = []
listaDeAmigos (x:xs) = listaAmigosAux x xs ++ listaDeAmigos xs

listaAmigosAux :: Int -> [Int] -> [(Int,Int)]
listaAmigosAux _ [] = []
listaAmigosAux n (x:xs) | sonAmigos n x = (n,x) : listaAmigosAux n xs
                        | otherwise = listaAmigosAux n xs

-- codificar parcial 
--1
hayQueCodificar :: Char -> [(Char,Char)] ->Bool
hayQueCodificar _ [] = False
hayQueCodificar c (mapeo:ms) | c == fst mapeo = True
                             | otherwise= hayQueCodificar c ms

--2
cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar _ [] _ = 0
cuantasVecesHayQueCodificar _ _ [] = 0
cuantasVecesHayQueCodificar c frase mapeo | hayQueCodificar c mapeo == False || qVeces c frase == 0 = 0
                                          | otherwise = qVeces c frase

qVeces :: Char -> [Char] -> Int
qVeces _ [] = 0
qVeces c (frase:fs) | c==frase = 1 + qVeces c fs
                    | otherwise= qVeces c fs

--3
laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar [c] _ = c
laQueMasHayQueCodificar (c:cs) mapeo | cuantasVecesHayQueCodificar c (c:cs) mapeo >= cuantasVecesHayQueCodificar (laQueMasHayQueCodificar cs mapeo) (c:cs) mapeo = c
                                     | otherwise = laQueMasHayQueCodificar cs mapeo

--4

codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase _ [] = []
codificarFrase [] _ = []
codificarFrase (f:fs) mapeo | hayQueCodificar f mapeo == False = f : codificarFrase fs mapeo
                            | hayQueCodificar f mapeo == True= codificarAux f mapeo : codificarFrase fs mapeo

codificarAux :: Char -> [(Char,Char)] -> Char
codificarAux f (mapeo:ms) | f == fst mapeo = snd mapeo 
                          | otherwise= codificarAux f ms

-- recu año pasado

{-1) Cantidad de caracteres en minúscula [2 puntos]
problema cantMinuscula (mensaje: String) : Z {
  requiere: {True}
  asegura: {res = cantidad de caracteres en minúscula en mensaje}-}

esMin :: Char -> Bool
esMin a = ord a >= ord 'a' && ord a <= ord 'z'
charANat :: Char -> Int
charANat a | esMin a = (ord a) - (ord 'a')
natAChar :: Int -> Char
natAChar n | 0 <= n && n <= 25 = chr ((ord 'a' )+n)
natAChar1 :: Int -> Char
natAChar1 n | (-25) <= n && n < 0 = chr ((ord 'z' )+n)

cantMinuscula :: [Char] -> Int
cantMinuscula [] = 0
cantMinuscula (x:xs) | esMin x == True = 1 + cantMinuscula xs
                     | otherwise = cantMinuscula xs 

-- long :: [Char] -> Int
-- long (c:cs) = 1 + minusAux cs

{-2) Mensaje con cantidad de cambios máxima [3 puntos]
problema maximoCambios (mensajes: seq< String >) : String {
  requiere: {| mensajes | > 0}
  asegura: {res = mensaje perteneciente a mensajes tal que la cantidad de cambios (letras minúsculas a reemplazar) que tienen que hacerse para codificarlo es máxima. En caso de haber más de un mensaje máximo, res puede ser cualquiera de ellos.}
}-}

maximoCambios :: [[Char]] -> [Char]
maximoCambios [] = []
maximoCambios (m:ms) = maxAux m ms

maxAux ::[Char] -> [[Char]] -> [Char]
maxAux [] _ = []
maxAux mensaje [] = mensaje
maxAux mensaje (m:ms) | cantMinuscula mensaje >= cantMinuscula m = maxAux mensaje ms
                      | otherwise= maxAux m ms

{-3) Desplazar [2 puntos]
problema desplazar (a: Char, n: Z) : Char {
  requiere: {-25 <= n <= 25}
  asegura: {ord a >= ord 'a' ∧ ord a <= ord 'z' ∧ 0 <= n <= 25 → res es el caracter que se encuentra a n posiciones más adelante en el alfabeto (si se llega al final se comienza desde el principio)}
  asegura: {ord a >= ord 'a' ∧ ord a <= ord 'z' ∧ -25 <= n < 0 → res es el caracter que se encuentra a n posiciones más atrás en el alfabeto (si se llega al principio se comienza desde el final)}
  asegura:listo {¬ (ord a >= ord 'a' ∧ ord a <= ord 'z') → res = a } 
-}-}

desplazar :: Char -> Int -> Char
desplazar a n | esMin a = desplazo (desplazoAux (despAux a n))
              | otherwise= a

despAux :: Char -> Int -> Int
despAux c n = mod (charANat c+n) 26

desplazo :: Int -> Char
desplazo c = natAChar c
 
desplazoAux :: Int -> Int
desplazoAux n | n < 0 = n + 26
              | otherwise = n

{-4) Codificar mensaje [2 puntos]
problema codificar (mensaje: String, n: Z) : String {
  requiere: {0 <= n <= 25}
  asegura: {res = versión codificada del mensaje, donde cada caracter en minúscula del mensaje se sustituye por la letra minúscula que se encuentra n posiciones más adelante en el alfabeto. Los caracteres que no son minúscula no se sustituyen.}
}-}

codificar :: [Char] -> Int -> [Char]
codificar [] _ = []
codificar msj n = codiAux msj n

codiAux :: [Char] -> Int -> [Char]
codiAux [] _ = []
codiAux (c:cs) n | esMin c == True = (desplazar c n) : codiAux cs n
                 | otherwise= c : codiAux cs n

{-5) Decodificar mensaje [1 puntos]
problema decodificar (mensaje: String, n: Z) : String {
  requiere: {0 <= n <= 25}
  asegura: {res = versión decodificada del mensaje, donde cada caracter en minúscula del mensaje se 
  sustituye por la letra minúscula que se encuentra n posiciones más atrás en el alfabeto. Los caracteres que no son minúscula no se sustituyen.}-}

decodificar :: [Char] -> Int -> [Char]
decodificar [] _ = []
decodificar (c:cs) n | esMin c == True = (desplazar c (-n)) : decodificar cs n
                     | otherwise= c : decodificar cs n

--Parcial stock q m tomaron a mi

generarStock :: [[Char]] -> [([Char],Int)]
generarStock [] = []
generarStock (p:ps) = (p,1 + qVecesQueAparece p ps) : generarStock (eliminar p ps)

qVecesQueAparece :: [Char] -> [[Char]] -> Int
qVecesQueAparece [] _ = 0
qVecesQueAparece _ [] = 0
qVecesQueAparece prod (p:ps) | prod == p = 1 + qVecesQueAparece prod ps
                             | otherwise= qVecesQueAparece prod ps

eliminar :: [Char] -> [[Char]] -> [[Char]]
eliminar _ [] = []
eliminar [] _ = []
eliminar s (x:xs) |s==x= eliminar s xs
                  | otherwise= x : eliminar s xs

--2 
stockDeProducto :: [([Char],Int)] -> [Char] ->Int
stockDeProducto [] _ = 0
stockDeProducto _ [] = 0
stockDeProducto (s:sx) stock | fst s == stock = snd s
                             | otherwise= stockDeProducto sx stock

--3
dineroEnStock :: [([Char],Int)] -> [([Char],Float)] -> Float
dineroEnStock [] _ = 0.0
dineroEnStock _ [] = 0.0
dineroEnStock (x:xs) (y:ys) = fromIntegral (snd x) * calculo (fst x) (y:ys) + dineroEnStock xs ys

calculo :: [Char] -> [([Char],Float)] -> Float
calculo _ [] = 0.0
calculo [] _ = 0.0
calculo s (x:xs) | s == fst x = snd x
                 | otherwise = calculo s xs

--4
aplicarOferta :: [([Char],Int)] -> [([Char],Float)] -> [([Char],Float)]
aplicarOferta [] precios = precios
aplicarOferta _ [] = []
aplicarOferta stock (x:xs) | stockDeProducto stock (fst x) > 10 = (fst x, 0.8 * snd x): aplicarOferta stock xs
                           | otherwise = x: aplicarOferta stock xs 