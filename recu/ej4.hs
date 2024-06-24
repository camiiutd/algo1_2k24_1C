aproboMasDeNMaterias :: [([Char], [Int])]-> [Char] ->Int-> Bool
aproboMasDeNMaterias [] _ _ = False
aproboMasDeNMaterias (registro:xs) alumno n | alumno == fst registro &&  (n >= (condicionmayoracuatro (snd registro))) ==True = True
                                            | alumno == fst registro &&  (n < (condicionmayoracuatro (snd registro))) == False =False
                                            | otherwise = aproboMasDeNMaterias xs alumno n

condicionmayoracuatro :: [Int] -> Int
condicionmayoracuatro [] = 0
condicionmayoracuatro (x:xs) | x >= 4 = 1 + condicionmayoracuatro xs
                             | otherwise= condicionmayoracuatro xs 
                             
sumanotas :: [Int] -> Int
sumanotas [] = 0
sumanotas (x:xs) = x + sumanotas xs

qNotas :: [Int] -> Int
qNotas [] = 0
qNotas (x:xs) = 1 + qNotas xs

promedioNotas :: [Int] -> Int
promedioNotas n = div (sumanotas n) (qNotas n)

aplazos:: [Int] ->Bool
aplazos [] = False
aplazos (x:xs) | x < 4 = True
               | otherwise= aplazos xs

buenosAlumnos :: [([Char], [Int])] -> [[Char]]
buenosAlumnos [] = []
buenosAlumnos (registro:xs) | promedioNotas (snd registro) >= 8 && aplazos (snd registro) == False = fst registro : buenosAlumnos xs
                            | otherwise = buenosAlumnos xs


--3
mejorpromedio :: [([Char], [Int])]->[Char]
mejorpromedio [] = []
mejorpromedio [registro] = fst registro
mejorpromedio (registro:registro2:xs) | promedioNotas (snd registro) >= promedioNotas (snd registro2) = mejorpromedio (registro:xs)
                                      | otherwise = mejorpromedio (registro2:xs)                            

-- buenosAlumnos


seGraduoConHonores :: [([Char], [Int])] -> Int -> [Char] -> Bool
seGraduoConHonores [] _ _ = False
seGraduoConHonores registro n alumno = aproboMasDeNMaterias registro alumno (n-1) &&  perteneceBuenosAlumnos alumno (buenosAlumnos registro) && condicionDifUno (mejorpromedioNota registro) (promedioNotas registro alumno)


perteneceBuenosAlumnos :: [Char]-> [[Char]] -> Bool
perteneceBuenosAlumnos _ [] = False
perteneceBuenosAlumnos alumno (x:xs) | alumno == x = True
                                     | otherwise= perteneceBuenosAlumnos alumno xs

promedioF :: [Int] -> Float
promedioF n = (fromIntegral (sumanotas n)) / (fromIntegral (qNotas n))

condicionDifUno :: [Int] -> [Int] -> Bool
condicionDifUno n k = (promedioF n - promedioF k) < 1 

mejorpromedioNota :: [([Char], [Int])]-> Float
mejorpromedioNota [] = 0
mejorpromedioNota [registro] = promedioNotas (snd registro)
mejorpromedioNota (registro:registro2:xs) | promedioF (snd registro) >= promedioF (snd registro2) = mejorpromedioNota (registro:xs)
                                          | otherwise = mejorpromedioNota (registro2:xs)