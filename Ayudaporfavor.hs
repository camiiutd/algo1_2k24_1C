module Ayudaporfavor where

hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar c ((a,b):xs) | c == a = True
                             | otherwise = hayQueCodificar c xs

----

cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar _ [] [] = 0
cuantasVecesHayQueCodificar _ _ [] = 0
cuantasVecesHayQueCodificar _ [] _ = 0
cuantasVecesHayQueCodificar c (frase:xs) ((mapeo,b):ys) | hayQueCodificar c ((mapeo,b):ys) == False = 0
                                                        | otherwise = cantidadApariciones c xs

cantidadApariciones :: Char -> [Char] -> Int
cantidadApariciones _ [] = 0
cantidadApariciones caracter (x:xs) | caracter == x = 1 + cantidadApariciones caracter xs
                                   | otherwise = cantidadApariciones caracter xs

-------------

laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar frase mapeo = ylaQueAuxilie frase frase mapeo 

ylaQueAuxilie :: [Char] -> [Char] -> [(Char,Char)] -> Char
ylaQueAuxilie [x] _ _ = x
ylaQueAuxilie (x:y:xs) frase mapeo | x == y = ylaQueAuxilie (x:xs) frase mapeo
                                   | cuantasVecesHayQueCodificar x frase mapeo >= cuantasVecesHayQueCodificar y frase mapeo = ylaQueAuxilie (x:xs) frase mapeo 
                                   | otherwise = ylaQueAuxilie (y:xs) frase mapeo

-------------
codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase (x:xs) mapeo | hayQueCodificar x mapeo = cambiaschar x mapeo : codificarFrase xs mapeo
                            | otherwise = x : codificarFrase xs mapeo


cambiaschar :: Char -> [(Char,Char)] -> Char 
cambiaschar frase ((a,b):xs) | frase == a = b 
                             | otherwise = cambiaschar frase xs 

-----parcial comi 3

aproboMasDeNMaterias :: [([Char], [Int])] -> [Char] -> Int -> Bool
aproboMasDeNMaterias [] _ _ = False
aproboMasDeNMaterias ((a,b):xs) alumno n | a == alumno && aprobados b > n = True
                                         | otherwise = aproboMasDeNMaterias xs alumno n

aprobados :: [Int] -> Int
aprobados [] = 0
aprobados (x:xs) | x >= 4 = 1 + aprobados xs
                 | otherwise = aprobados xs

-----2
buenosAlumnos :: [([Char], [Int])] -> [[Char]]-- <- en el fondo es seq de string
buenosAlumnos [] = []
buenosAlumnos ((registro,b):xs) | condicionprom (promedio b) && condicionAplazo b = registro : buenosAlumnos xs
                                | otherwise = buenosAlumnos xs

condicionAplazo :: [Int] -> Bool
condicionAplazo [] = True
condicionAplazo (x:xs) | x < 4 = False
                       | x >= 4 = condicionAplazo xs

condicionprom :: Float -> Bool
condicionprom promfinal = promfinal >= 8 

promedio :: [Int] -> Float
promedio b = fromIntegral (sumatoria b) / fromIntegral (qDeNotas b)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

qDeNotas :: [Int] -> Int
qDeNotas [] = 0
qDeNotas (x:xs) = 1 + qDeNotas (xs)

----3
mejorPromedio :: [([Char], [Int])] -> [Char]
mejorPromedio [] = []
mejorPromedio [(registro,_)] = registro
mejorPromedio ((registro,b):(registro1,c):xs) | promedio b >= promedio c = mejorPromedio ((registro,b):xs)
                                              | otherwise = mejorPromedio ((registro1,c):xs)

----5
seGraduoConHonores :: [([Char], [Int])] -> Int -> [Char] -> Bool
seGraduoConHonores [] _ _ = False
seGraduoConHonores ((registro, notas):registrosRestantes) n alumno | primeraCondicion ((registro, notas):registrosRestantes) n alumno && segundaCondicion ((registro, notas):registrosRestantes) alumno && terceraCondicion notas = True
                                                                   | otherwise = seGraduoConHonores registrosRestantes n alumno

primeraCondicion :: [([Char], [Int])] -> Int -> [Char] -> Bool
primeraCondicion [] _ _ = False
primeraCondicion ((registro, notas):registrosRestantes) n alumno
    | registro == alumno && aprobadasAlumno notas >= n = True
    | otherwise = primeraCondicion registrosRestantes n alumno

segundaCondicion :: [([Char], [Int])] -> [Char] -> Bool
segundaCondicion [] _ = False
segundaCondicion ((registro, b):registrosRestantes) alumno
    | registro == alumno && pertenece registro (buenosAlumnos ((registro, b):registrosRestantes)) = True
    | otherwise = segundaCondicion registrosRestantes alumno

terceraCondicion :: [Int] -> Bool
terceraCondicion [] = False
terceraCondicion notas = promedio notas < mejorPromedioDis notas

aprobadasAlumno :: [Int] -> Int
aprobadasAlumno [] = 0
aprobadasAlumno (nota:notasRestantes)
    | nota >= 6 = 1 + aprobadasAlumno notasRestantes
    | otherwise = aprobadasAlumno notasRestantes

mejorPromedioDis :: [Int] -> Float
mejorPromedioDis [] = 0
mejorPromedioDis notas = head (mejorPromedioDisAux notas)

mejorPromedioDisAux :: [Int] -> [Float]
mejorPromedioDisAux [] = []
mejorPromedioDisAux [_] = []
mejorPromedioDisAux (x:y:xs)
    | promedio [x] > promedio [y] = promedio [x] : mejorPromedioDisAux (x:xs)
    | otherwise = promedio [y] : mejorPromedioDisAux (y:xs)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece y (x:xs)
    | y == x = True
    | otherwise = pertenece y xs

