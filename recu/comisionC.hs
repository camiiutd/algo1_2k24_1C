{--El Departamento de Matemática (DM) de la FCEyN-UBA nos ha encargado que desarrollemos un sistema para el
tratamiento de números naturales. Específicamente les interesa conocer cuándo un número es perfecto y
cuándo dos números son amigos. Aunque por ahí no lo sabías, estos conceptos existen y se definen como:

Un número natural es perfecto cuando la suma de sus divisores propios (números que lo dividen menores a él)
es igual al mismo número. Por ejemplo, 6 es un número perfecto porque la suma de sus divisores propios (1,2 y 3)
es igual a 6.
Dos números naturales distintos son amigos si cada uno de ellos se obtiene sumando los divisores propios del
otro. Por ejemplo, 220 y 284 son amigos porque los divisores propios de 220 son 1, 2, 4, 5, 10, 11, 20, 22, 44,
55 y 110 que sumados dan 284 y los divisores propios de 284 son 1, 2 , 4, 71, 142 que sumados dan 220.
Para implementar este sistema nos enviaron las siguientes especificaciones en lenguaje semiformal y nos pidieron
 que hagamos el desarrollo enteramente en Haskell, utilizando los tipos requeridos y solamente las funciones que
se ven en la materia Introducción a la Programación / Algoritmos y Estructuras de Datos I (FCEyN-UBA).
--}
{--Ejercicio 1
problema divisoresPropios (n: Z) : seq⟨Z⟩ {
  requiere: {n > 0}
  asegura: {res es la lista de divisores propios de n, de menor a mayor.}
}
--}

auxpropios :: Int -> Int -> [Int]
auxpropios x y | y>=x = []
               | mod x y == 0 = y:  auxpropios x (y+1)
               | otherwise = auxpropios x (y+1)

divisoresPropios :: Int -> [Int]
divisoresPropios n = auxpropios n 1

{--Ejercicio 2
problema sonAmigos (n: Z, m: Z) : Bool {
  requiere: {n > 0, m >0 y m es distinto de n}
  asegura: {(res = true <=> n y m son números amigos}
}
--}

sonAmigos :: Int -> Int -> Bool
sonAmigos n m = sumaLista (divisoresPropios n) == m && sumaLista (divisoresPropios m) == n 

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

{--Ejercicio 3
problema losPrimerosNPerfectos (n: Z) : seq⟨Z⟩ {
  requiere: {n > 0}
  asegura: {res es la lista de los primeros n números perfectos, de menor a mayor.}
}
--}
esnumperfecto :: Int-> [Int] -> Bool
esnumperfecto _ [] =False
esnumperfecto n (x:xs) = n== sumaLista (x:xs) 

encontrarPerfectos :: Int -> Int -> [Int]
encontrarPerfectos 0 _ = []
encontrarPerfectos n a | esnumperfecto a (divisoresPropios a) = a : encontrarPerfectos (n-1) (a+1)
                       | otherwise = encontrarPerfectos n (a+1)

losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos n = encontrarPerfectos n 1


{--Ejercicio 4
problema listaDeAmigos (lista: seq⟨Z⟩) : seq⟨Z x Z⟩{
  requiere: {todos los números de lista son mayores a 0}
  requiere: {todos los números de lista son distintos}
  asegura: {res es una lista sin repetidos de tuplas de dos números donde esos dos números pertenecen a lista y son amigos}
  asegura: {|res| es la cantidad de tuplas de dos números amigos que hay en lista. Consideraremos que la tupla (a,b) (con a y b pertenecientes a Z) es igual a la tupla (b,a) para contar la cantidad de tuplas.}
}
--}



listaDeAmigos :: [Int] -> [(Int,Int)]
listaDeAmigos [] = []
listaDeAmigos (n:ns) | pertenece (sumaLista (divisoresPropios n)) ns = (n,elAmigo) : listaDeAmigos ns
                     | otherwise= listaDeAmigos ns
                     where elAmigo = (sumaLista (divisoresPropios n))

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise= pertenece n xs

-- listaAmigosAux ::  [Int] -> Bool
-- listaAmigosAux _ [] =False
-- listaAmigosAux  (x:xs) = pertenece (sumaLista (divisoresPropios x) ) xs

-- listaAux :: Int -> [Int] -> Int
-- listaAux _ [] =0
-- listaAux n (x:xs) | n == x = x
--                   | otherwise= listaAux n xs