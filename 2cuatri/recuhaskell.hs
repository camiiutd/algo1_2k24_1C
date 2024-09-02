import Distribution.Types.IncludeRenaming (IncludeRenaming)
type Fila = [Int]
type Tablero = [Fila]
type Posicion = (Int,Int)
type Camino = [Posicion]

{-Ejercicio 1 (2 puntos)
problema maximo (t: Tablero) : Z {
  requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al menos un elemento}
  requiere: {Existe al menos una columna en el tablero t }
  requiere: {El tablero t no es vacío, todos los números del tablero son positivos, mayor estricto a 0}
  asegura: {res es igual al número más grande del tablero t}
}
-}

--1
maximo :: Tablero -> Int
maximo [[x]] = x 
maximo [] = 0
maximo t = comparoMaximos t

tomoMaximoLista :: [Int] ->Int
tomoMaximoLista [x] = x
tomoMaximoLista (x:xs) | x >= tomoMaximoLista xs = x
                       | otherwise= tomoMaximoLista xs

comparoMaximos :: [[Int]] -> Int
comparoMaximos [[x]] =x 
comparoMaximos [] = 0
comparoMaximos (x:xs) | tomoMaximoLista x >= comparoMaximos xs = tomoMaximoLista x
                      | otherwise= comparoMaximos xs

--2 
{-Ejercicio 2 (2 puntos)
problema masRepetido (t: Tablero) : Z {
  requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al menos un elemento}
  requiere: {Existe al menos una columna en el tablero t }
  requiere: {El tablero t no es vacío, todos los números del tablero son positivos, mayor estricto a 0}
  asegura: {res es igual al número que más veces aparece en un tablero t. Si hay empate devuelve cualquiera de ellos}
}-}

masRepetido :: Tablero -> Int
masRepetido t = elQueMasRep (flattenTablero t)

flattenTablero :: Tablero -> [Int]
flattenTablero [] = []
flattenTablero (fila:resto) = aplanarFila fila ++ flattenTablero resto

aplanarFila :: [Int] -> [Int]
aplanarFila [] = []
aplanarFila (x:xs) = x : aplanarFila xs

seRepite :: Int -> [Int] -> Int
seRepite _ [] = 0
seRepite n (x:xs) | n == x = 1 + seRepite n xs
                  | otherwise = seRepite n xs

elQueMasRep :: [Int] -> Int
elQueMasRep [] = 0
elQueMasRep [x] = x
elQueMasRep (x:xs) | seRepite x (x:xs) >= seRepite (elQueMasRep xs) (x:xs) = x
                   | otherwise = elQueMasRep xs

--3
{-Ejercicio 3 (2 puntos)
problema valoresDeCamino (t: Tablero, c: Camino) : seq⟨Z⟩ {
  requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al menos un elemento}
  requiere: {Existe al menos una columna en el tablero t }
  requiere: {El tablero t no es vacío, todos los números del tablero son positivos, mayor estricto a 0}
  requiere: {El camino c es un camino válido, es decir, secuencia de posiciones adyacentes en la que solo es posible desplazarse hacia la posición de la derecha o hacia abajo y todas las posiciones están dentro de los limites del tablero t}
  asegura: {res es igual a la secuencia de números que están en el camino c, ordenados de la misma forma que aparecen las posiciones correspondientes en el camino.}
}-}

-- valoresDeCamino :: Tablero -> Camino -> [Int]

buscoColumna :: [Int] -> Int -> Int
buscoColumna (x:xs) 0 = x
buscoColumna (x:xs) columna = buscoColumna xs (columna-1)




-- --Ejercicio 4 (2 puntos)
-- problema esCaminoFibo (s: seq⟨Z⟩, i: Z) : Bool {
--   requiere: {La secuencia de números s es no vacía y está compuesta por números positivos (mayor estricto que 0) que representan los valores de un camino en un tablero}
--   requiere: {i >= 0}
--   asegura: {res = true <=> los valores de s son la sucesión de Fibonacci inicializada con el número pasado como parámetro i}
-- }
-- Notas:
-- En este ejercicio se pasa una secuencia de valores en lugar de un tablero y un camino para no generar dependencia con el ejercicio anterior.
-- Recordemos que la sucesión de Fibonacci está definida con la siguiente función recursiva:
-- f(0) = 0
-- f(1) = 1
-- f(n) = f(n-1) + f(n-2) con n>1
-- En el ejemplo del tablero y del camino (verde claro) que figuran más arriba tenemos que esCaminoFibo [1,1,2,3,5] 1 reduce a True.

