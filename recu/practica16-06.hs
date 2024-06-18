--parcial comiA

hayQueCodificar :: Char -> [(Char,Char)] ->Bool
hayQueCodificar _ [] = False
hayQueCodificar c (mapeo:xs) | c == fst mapeo = True
                             | otherwise= hayQueCodificar c xs

--2
cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)]->Int
cuantasVecesHayQueCodificar _ _ [] = 0
cuantasVecesHayQueCodificar c frase (mapeo:xs) | hayQueCodificar c (mapeo:xs) == False= 0
                                               | otherwise= qVeces c frase

qVeces :: (Eq t) => t -> [t] -> Int
qVeces _ [] = 0
qVeces c (x:xs) | c == x = 1+ qVeces c xs
                | otherwise= qVeces c xs
----3

laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar [c] (mapeo:xs) = c
laQueMasHayQueCodificar (c:cs) (mapeo:xs) | cuantasVecesHayQueCodificar c (c:cs) (mapeo:xs) >= cuantasVecesHayQueCodificar (laQueMasHayQueCodificar cs (mapeo:xs)) (c:cs) xs = c
                                          | otherwise= laQueMasHayQueCodificar cs xs
 
--4
codificarFrase :: [Char] -> [(Char,Char)]-> [Char]
codificarFrase [] _ = []
codificarFrase (frase:fs) (mapeo:xs) = condicion2 frase (mapeo:xs) : codificarFrase fs (mapeo:xs)


condicion2 :: Char -> [(Char,Char)] -> Char
condicion2 c []= c
condicion2 c ((x,y):xs) | c == x = y
                        | otherwise= condicion2 c xs
{-
maximo :: (Eq t, Ord t, Num t) => t -> [t] -> t
maximo c (x:xs) | c >= x = c
                | otherwise= maximo c xs 

separoString :: (Eq t) => [t] -> [t]
separoString [] = []
separoString (palabra:xs) = head (palabra:xs) : separoString xs
-}

--parcial comi C

aproboMasDeNMaterias :: [([Char], [Int])]-> [Char] ->Int-> Bool
aproboMasDeNMaterias [] _ _ = False
aproboMasDeNMaterias (registro:xs) alumno n | alumno == fst registro && condicionmayoracuatro (snd registro) >= 4 = True
                                            | alumno == fst registro && condicionmayoracuatro (snd registro) < 4 =False
                                            | otherwise = aproboMasDeNMaterias xs alumno n

condicionmayoracuatro :: [Int] -> Int
condicionmayoracuatro [] = 0
condicionmayoracuatro (x:xs) | x >= 4 = 1 + condicionmayoracuatro xs
                             | otherwise= condicionmayoracuatro xs 


--2
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


--4
perteneceBuenosAlumnos:: [([Char], [Int])] -> [Char] -> Bool
perteneceBuenosAlumnos [] _ =False
perteneceBuenosAlumnos (registro:xs) nombre | fst registro == nombre && promedioNotas (snd registro) >= 8 && aplazos (snd registro) == False = True
                                            | otherwise = perteneceBuenosAlumnos xs nombre

mejorPnumerito :: [([Char], [Int])] -> Int
mejorPnumerito [] = 0
mejorPnumerito (registro:registro2:xs) | promedioNotas (snd registro) >= promedioNotas (snd registro2) = promedioNotas (snd registro)
                                       | otherwise = mejorPnumerito (registro:registro2:xs)

seGraduoConHonores :: [([Char], [Int])] -> Int -> [Char] -> Bool
seGraduoConHonores [] _ _ = False
seGraduoConHonores (registro:xs) n alumno | aproboMasDeNMaterias (registro:xs) alumno n == True && perteneceBuenosAlumnos (registro:xs) alumno == True && (mejorPnumerito (registro:xs)) < (promedioNotas (snd registro))+1 = True
                                          | otherwise = seGraduoConHonores xs n alumno

--seGraduoConHonores [("Juan", [6, 8, 9]), ("Ana", [10, 9, 8]), ("Luis", [5, 4, 7]), ("Maria", [10, 10, 10]), ("Jose", [7, 6, 6]), ("Laura", [9, 9, 9, 4])] 2 "Ana"
