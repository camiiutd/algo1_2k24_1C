-- Parcial notas de alumnos --
{-La Unidad de Tecnologías de la Información (UTI) de nuestra Facultad nos ha encargado que desarrollemos un nuevo sistema para el registro de alumnos. En este sistema 
se guarda la información de cada alumno, que está representada como una tupla de dos elementos: el primero es el nombre completo del alumno y el segundo una lista 
con las notas de los finales que rindió.

Para implementar este sistema nos enviaron las siguientes especificaciones y nos pidieron que hagamos el desarrollo enteramente en Haskell, utilizando los tipos requeridos 
y solamente las funciones que se ven en la materia Introducción a la Programación / Algoritmos y Estructuras de Datos I (FCEyN-UBA).

Ejercicio 1 (2 puntos) 
problema aproboMasDeNMaterias (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩, alumno:seq⟨Char⟩, n: Z) : Bool {
  requiere: {No hay nombres de alumnos repetidos en registro}
  requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  requiere: {n > 0}
  requiere: {El alumno se encuentra en el registro }
  asegura: {res = true <=> el alumno tiene más de n notas de finales mayores o iguales a 4 en el registro}
}-}

aproboMasDeNMaterias :: [([Char], [Int])]-> [Char] -> Int -> Bool
aproboMasDeNMaterias [] _ _ = False
aproboMasDeNMaterias (x:xs) alum n | fst x == alum && contadorDeAprobados (snd x) > n = True
                                   | otherwise= aproboMasDeNMaterias xs alum n 

contadorDeAprobados :: [Int] -> Int
contadorDeAprobados [] = 0
contadorDeAprobados (x:xs) | x >= 4 = 1 + contadorDeAprobados xs
                           | otherwise = contadorDeAprobados xs 

--[("Juan Perez", [9, 10, 10, 10]), ("Maria Lopez", [7, 9, 3, 5]), ("Pedro Gomez", [10,10,10,10]), ("Ana Martinez", [10, 8, 7, 9])]

{-Ejercicio 2 (2 puntos)
problema buenosAlumnos (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩) : seq⟨seq⟨Char⟩⟩ {
  requiere: {No hay nombres de alumnos repetidos en registro}
  requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  asegura: {res es la lista de los nombres de los alumnos que están en registro cuyo promedio de notas es mayor o igual a 8 y no tiene aplazos (notas menores que 4)}
}
Para resolver el promedio pueden utilizar la función del Preludio de Haskell fromIntegral que dado un valor de tipo Int devuelve su equivalente de tipo Float.-}

buenosAlumnos :: [([Char],[Int])]->[[Char]]
buenosAlumnos [] = []
buenosAlumnos (x:xs) | promedio (sumoNotas (snd x)) (cantidadNotas (snd x)) >= 8 && pertenece 4 (snd x) == False = (fst x ) : buenosAlumnos xs 
                     | otherwise= buenosAlumnos xs

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise= pertenece n xs

sumoNotas :: [Int] -> Int
sumoNotas [] =0
sumoNotas (x:xs) = x + sumoNotas xs 

cantidadNotas :: [Int] -> Int
cantidadNotas [] = 0
cantidadNotas (x:xs) = 1+ cantidadNotas xs 

promedio :: Int -> Int -> Float
promedio n k = (fromIntegral n )/ (fromIntegral k )

{-Ejercicio 3 (2 puntos)
problema mejorPromedio (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩) : seq⟨Char⟩ {
  requiere: {No hay nombres de alumnos repetidos en registro}
  requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  requiere: {|registro| > 0 }
  asegura: {res es el nombre del alumno cuyo promedio de notas es el más alto; si hay más de un alumno con el mismo promedio de notas, devuelve el nombre de alumno que aparece primero en registro}
}-}

mejorPromedio :: [([Char],[Int])]->[Char]
mejorPromedio [x] = fst x
mejorPromedio (x:xs) | promedio (sumoNotas (snd x)) (cantidadNotas (snd x)) >=  (maximo xs) = fst x
                     | otherwise= mejorPromedio xs 

maximo :: [([Char],[Int])] -> Float
maximo [] = 0
maximo [x] = (promedio (sumoNotas (snd x)) (cantidadNotas (snd x)))
maximo (x:xs) | promedio (sumoNotas (snd x)) (cantidadNotas (snd x)) >= (maximo xs) = promedio (sumoNotas (snd x)) (cantidadNotas (snd x))
              | otherwise= maximo xs 

{-Ejercicio 4 (3 puntos)
problema seGraduoConHonores (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩, cantidadDeMateriasDeLaCarrera: Z, alumno: seq⟨Char⟩ ) : Bool {
  requiere: {No hay nombres de alumnos repetidos en registro}
  requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
  requiere: {cantidadDeMateriasDeLaCarrera > 0}
  requiere: {El alumno se encuentra en el registro }
  requiere: {|buenosAlumnos(registro)| > 0}
  asegura: {res <=> true si aproboMasDeNMaterias(registro, alumno, cantidadDeMateriasDeLaCarrera -1) = true y alumno pertenece al conjunto de buenosAlumnos(registro) y el promedio de notas de finales de alumno está a menos (estrictamente) de 1 punto del mejorPromedio(registro)}
}-}

seGraduoConHonores :: [([Char],[Int])] -> Int -> [Char] -> Bool
seGraduoConHonores [] _ _ =False
seGraduoConHonores (x:xs) n alum | aproboMasDeNMaterias (x:xs) alum (n -1) == True && perteneceAlumno alum (buenosAlumnos (x:xs)) && (promedio (sumoNotas (snd x)) (cantidadNotas (snd x)) / sacoPromedioMejorAlumno (x:xs)) > 1 = True
                                 | otherwise= seGraduoConHonores xs n alum

sacoPromedioMejorAlumno :: [([Char],[Int])] -> Float 
sacoPromedioMejorAlumno [] =0
sacoPromedioMejorAlumno (x:xs) | perteneceAlumno (mejorPromedio  (x:xs)) (buenosAlumnos(x:xs)) = promedio (sumoNotas (snd x)) (cantidadNotas (snd x)) 
                                 | otherwise= sacoPromedioMejorAlumno xs  

perteneceAlumno :: [Char] -> [[Char]] -> Bool
perteneceAlumno _ [] = False
perteneceAlumno n (x:xs) | n == x = True
                         | otherwise= perteneceAlumno n xs


{-Ejercicio 5 (1 punto)
Conteste marcando la opción correcta. El Testing es una técnica de verificación que sirve para:
○ Demostrar que un programa es correcto.
○ Probar propiedades de un programa.
● Encontrar fallas en un programa.--> ES ESTAAAAAAAAAAAAAAAAAAA -}