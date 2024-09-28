{-¡VAMOS CAMPEÓN!
En Exactas se está jugando un torneo de futbol y la facultad le pidió a los alumnos de IP programar algunas funcionalidades en Haskell.
Los datos con los que contamos para esto son los nombres de los equipos que participan del torneo, los nombres de los goleadores de cada
uno de dichos equipo, y la cantidad de goles convertidos por esos jugadores. Los nombres de los equipos y sus respectivos goleadores serán
modelados mediante tuplas de tipo (String,String), donde la primera componente representa el nombre del equipo, y la segunda representa el
nombre del goleador de dicho equipo.

En los problemas en los cuales se reciban, como parámetros, secuencias _goleadoresPorEquipo_ y _goles_, cada posicion de la lista goles representará
la cantidad de goles obtenidos por el goleador del equipo que se encuentra en esa misma posición de _goleadoresPorEquipo_.
Por ejemplo si la lista goleadoresPorEquipo es [("Sacachispas","Robertino Giacomini"),("Fénix","Matias Dominguez")] y la lista goles es [3,5], eso indica
que Robertino Giacomini metió 3 goles y Matias Dominguez metió 5.-}

{-1) Goles de no goleadores [1 punto]

problema golesDeNoGoleadores (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩, totalGolesTorneo: Z ): Z {
    requiere: {equiposValidos(goleadoresPorEquipo)}
    requiere: {|goleadoresPorEquipo| = |goles|}
    requiere: {Todos los elementos de goles son mayores o iguales a 0}
    requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
    asegura: {res es la cantidad de goles convertidos en el torneo por jugadores que no son los goleadores de sus equipos}
}
-}
golesDeNoGoleadores :: [(String,String)] -> [Int] -> Int ->Int
golesDeNoGoleadores [] _ _ =0
golesDeNoGoleadores _ [] _ =0
golesDeNoGoleadores g (x:xs) total = total -  sumoGoles (x:xs)

sumoGoles :: [Int] ->Int
sumoGoles [] =0
sumoGoles (x:xs) = x+sumoGoles xs 


-- [("aa","pp"),("ifi","ptf")]
{-
2) Equipos Válidos [3 puntos]

problema equiposValidos (goleadoresPorEquipo: seq⟨String x String⟩): Bool{
    requiere: {True}
    asegura: {(res = True) <-> goleadoresPorEquipo no contiene nombres de clubes repetidos, ni goleadores repetidos, ni jugadores con nombre de club}
}-}

equiposValidos :: [(String,String)] ->Bool
equiposValidos [] =True
equiposValidos (x:xs) | perteneceFst (fst x) xs = False
                      | perteneceFst (snd x) xs = False
                      | perteneceSnd (fst x) xs = False
                      | perteneceSnd (snd x) xs = False
                      | fst x == snd x = False
                      | otherwise= equiposValidos xs
 
perteneceFst :: String -> [(String,String)] ->Bool
perteneceFst _ [] = False
perteneceFst n (x:xs) | n == fst x = True
                      | otherwise= perteneceFst n xs

perteneceSnd :: String -> [(String,String)] ->Bool
perteneceSnd _ [] =False
perteneceSnd n (x:xs)  | n == snd x = True
                       | otherwise=perteneceSnd n xs

--
{-3) Porcentaje de Goles [3 puntos]

problema porcentajeDeGoles (goleador: String, goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): R {
    requiere: {La segunda componente de algún elemento de goleadoresPorEquipo es goleador}
    requiere: {equiposValidos(goleadoresPorEquipo)}
    requiere: {|goleadoresPorEquipo| = |goles|}
    requiere: {Todos los elementos de goles son mayores o iguales a 0}
    requiere: {Hay al menos un elemento de goles mayor estricto a 0}
    asegura: {res es el porcentaje de goles que marcó goleador sobre el total de goles convertidos por goleadores}
}

Se sugiere usar la funcion:
division :: Int -> Int -> Float
division n m = fromIntegral n / fromIntegral m-}

porcentajeDeGoles :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeGoles _ [] _ =0
porcentajeDeGoles _ _ [] = 0
porcentajeDeGoles g (x:xs) (y:ys) = division (goles g (x:xs) (y:ys)) (sumoGoles (y:ys)) *100

goles :: String -> [(String,String)] -> [Int] -> Int
goles _ [] _ = 0
goles _ _ [] =0
goles n (x:xs) (y:ys) | n == snd x = y 
                      | otherwise= goles n xs ys

division :: Int -> Int -> Float
division n m = fromIntegral n / fromIntegral m

{-4) Botín de Oro [3 puntos]

problema botinDeOro (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): String {
    requiere: {equiposValidos(goleadoresPorEquipo)}
    requiere: {|goleadoresPorEquipo| = |goles|}
    requiere: {Todos los elementos de goles son mayores o iguales a 0}
    requiere: {|goles| > 0}
    asegura: {res es alguno de los goleadores de goleadoresPorEquipo que más tantos convirtió de acuerdo a goles}
}-}

botinDeOro :: [(String,String)] -> [Int] -> String
botinDeOro [x] _ = snd x 
botinDeOro (x:xs) (y:ys) | y >= maximoGoleador (x:xs) ys = snd x 
                         | otherwise= botinDeOro xs ys

maximoGoleador :: [(String,String)] -> [Int] -> Int
maximoGoleador _ [y] = y
maximoGoleador (x:xs) (y:ys) | y >= maximoGoleador xs ys = y
                             | otherwise= maximoGoleador xs ys 