--PARCIAL VOTOS--
--Las fórmulas presidenciales serán representadas por tuplas (String x String), donde la primera componente será el nombre del candidato a presidente, 
--y la segunda componente será el nombre del candidato a vicepresidente.
--En los problemas en los cuales se reciban como parámetro secuencias de fórmulas y votos, cada posición de la lista votos representará la 
--cantidad de votos obtenidos por la fórmula del parámetro formulas en esa misma posición. Por ejemplo, si la lista de fórmulas 
--es [("Juan Pérez","Susana García"), ("María Montero","Pablo Moreno")] y la lista de votos fuera [34, 56], eso indicaría que la fórmula 
--encabezada por María Montero obtuvo 56 votos, y la lista encabezada por Juan Pérez obtuvo 34 votos.

{--1) Porcentaje de Votos Afirmativos [1 punto]
--problema porcentajeDeVotosAfirmativos (formulas: seq⟨String x String⟩,votos:seq< Z >, cantTotalVotos: Z) : R {
-- requiere: {¬formulasInvalidas(formulas)}
-- requiere: {|formulas| = |votos|}
-- requiere: {Todos los elementos de votos son mayores o iguales a 0}
-- requiere: {La suma de todos los elementos de votos es menor o igual a cantTotalVotos}
-- asegura: {res es el porcentaje de votos no blancos (es decir, asociados a alguna de las fórmulas) sobre el total de votos emitidos}
--}
--Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos números de tipo Int:

--division :: Int -> Int -> Float
--division a b = (fromIntegral a) / (fromIntegral b)}

porcentajeDeVotosAfirmativos :: [(String,String)] -> [Int] -> Int -> Float
porcentajeDeVotosAfirmativos [] _ _ = 0
porcentajeDeVotosAfirmativos _ [] _ = 0
porcentajeDeVotosAfirmativos (x:xs) votos q =( division (sumoVotos votos)  q ) *100

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

sumoVotos :: [Int] -> Int
sumoVotos [] = 0
sumoVotos (x:xs) = x + sumoVotos xs 

--2) Formulas Inválidas [3 puntos]
--problema formulasInvalidas (formulas: seq⟨String x String⟩) : Bool {
-- requiere: {True}
-- asegura: {(res = true) <=> formulas contiene un candidato q se propone para presidente y vicepresidente en la misma fórmula; o algún candidato 
--se postula para presidente o vice en más de una fórmula}

formulasInvalidas :: [(String,String)] -> Bool
formulasInvalidas [] = False
formulasInvalidas (x:xs) | fst x == snd x= True
                         | perteneceTuplafst (fst x) xs = True
                         | perteneceTuplasnd (fst x) xs = True
                         | perteneceTuplafst (snd x) xs = True
                         | perteneceTuplasnd (snd x) xs =True
                         | otherwise= formulasInvalidas xs

perteneceTuplafst :: String ->  [(String,String)] -> Bool
perteneceTuplafst _ [] = False
perteneceTuplafst n (x:xs) | n == fst x = True
                           | otherwise= perteneceTuplafst n xs

perteneceTuplasnd :: String ->  [(String,String)] -> Bool
perteneceTuplasnd _ [] = False
perteneceTuplasnd n (x:xs) | n == snd x = True
                           | otherwise= perteneceTuplasnd n xs

--[("aa","ee"),("er","aa")]

--3) Porcentaje de Votos [3 puntos]
--problema porcentajeDeVotos (vice: String, formulas: seq⟨String x String⟩,votos:seq< Z >) : R {
-- requiere: {La segunda componente de algún elemento de formulas es vice}
-- requiere: {¬formulasInvalidas(formulas)}
-- requiere: {|formulas| = |votos|}
-- requiere: {Todos los elementos de votos son mayores o iguales a 0}
-- requiere: {Hay al menos un elemento de votos mayores estricto a 0}
-- asegura: {res es el porcentaje de votos que obtuvo vice sobre el total de votos afirmativos}
--}
--Para resolver este ejercicio pueden utilizar la función division presentada en el Ejercicio 1.

porcentajeDeVotos :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeVotos _ [] _ =0
porcentajeDeVotos [] _ _ =0
porcentajeDeVotos vice (x:xs) (v:vs) = (division (votosvalidos vice (x:xs) (v:vs)) (sumoVotos (v:vs) )) *100
                                     

votosvalidos ::  String -> [(String,String)] -> [Int] -> Int
votosvalidos _ [] _ = 0
votosvalidos [] _ _ =0
votosvalidos n (x:xs) (y:ys) | n == snd x = y
                             | otherwise = votosvalidos n xs ys


--4) Menos Votado [3 puntos]
--problema menosVotado (formulas: seq⟨String x String⟩, votos:seq< Z >) : String {
-- requiere: {¬formulasInvalidas(formulas)}
-- requiere: {|formulas| = |votos|}
-- requiere: {Todos los elementos de votos son mayores o iguales a 0}
-- requiere: {Hay al menos un elemento de votos mayores estricto a 0}
-- requiere: {|formulas| > 0}
-- asegura: {res es el candidato a presidente de formulas menos votado de acuerdo a los votos contabilizados en votos}
--}

menosVotado :: [(String,String)] -> [Int] -> String
menosVotado [x] _ = fst x
menosVotado (x:xs) (y:ys) | y <= menosaux ys = fst x
                          | otherwise= menosVotado xs ys

menosaux ::  [Int] -> Int
menosaux [] =0
menosaux [y] = y
menosaux (y:ys) | y <= menosaux ys = y
                | otherwise= menosaux ys