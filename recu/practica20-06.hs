{-

¡Vamos Campeones!
En exactas se está jugando un torneo de fútbol y la facultad le pidió a los alumnos de IP programar algunas
funcionalidades en Haskell, Los datos con los que contamos para esto son los nombres de los equipos que participan
del torneo, los nombres de los arqueros titulares de cada uno de dichos equipos, y la cantidad de goles recibidos
por esos arqueros. Los nombres de los equipos y sus respectivos arqueros serán modelados mediante tuplas de tipo
(String, String), donde la primera componente representa el nombre del equipo, y la segunda representa el nombre
del arquero titular de dicho equipo.
En los problemas en los cuales se reciben como parámetros secuencias arquerosPorEquipo y goles, cada posición de
la lista goles representará la cantidad de goles recibidos por el arquero del equipo que se encuentra en esa misma
posicion de arquerosPorEquipo. Por ejemplo, si la lista arquerosPorEquipo es [("Sacachispas", "Neyder Aragon"),
("Fenix", "Nahuel Galardi")] y la lista de goleses [3, 5], eso indicaría que Neyder Aragon recibió 3 goles, y
Nahuel Galardi 5.

Se pueden usar las siguientes funciones del preludio:
	- Listas: head, tail, last, init, length, elem, ++
	- Tuplas: fst, snd
	- Operaciones Lógicas: &&, ||, not
	- Constructores de listas: (x:xs), []
	- Constructores de tuplas: (x, y)


2) Equipos Válidos
problema equiposValidos (arquerosPorEquipo: seq<String X String>): Bool {
	requiere: {True}
	asegura: {
	(res = True) <=> arquerosPorEquipo no contiene nombres de clubes repetidos, ni arqueros repetidos, ni jugadores con nombre del club
	}
}


3) Porcentaje de goles
problema porcentajeDeGoles (arquero: String, arquerosPorEquipo: seq<String X String>, goles: seq<Z>): R {
	requiere: {La segunda componente de algún elemento de arquerosPorEquipo es arquero}
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {Hay al menos un elemento de goles mayores estricto a 0}
	asegura: {
	res es el porcentaje de goles que recibió arquero sobre el total de goles recibidos por arqueros titulares
	}
}

Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como float la división entre dos
numeros de tipo Int.

division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b


4) Valla Menos Vencida
problema vallaMenosVencida (arquerosPorEquipo: seq<String X String>, goles: seq<Z>): String {
	requiere: {equiposValidos(arquerosPorEquipo)}
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {|goles| > 0}
	asegura: {
	res es alguno de los arqueros de arquerosPorEquipo que menor goles recibió de acuerdo a goles
	}
}

-}
{-1) Atajaron Suplentes
problema atajaronSuplentes (arquerosPorEquipo: seq<String X String>, goles: seq<Z>, totalGolesTorneo: Z): Z {
	requiere: {equiposValidos(arquerosPorEquipo)
	requiere: {|arquerosPorEquipo| = |goles|}
	requiere: {Todos los elementos de goles son mayores o iguales a 0}
	requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
	asegura: {
	res es la cantidad de goles recibidos en el torneo por arqueros que no son titulares en sus equipos.
	}
}-}

arquerosPorEquipo :: [([Char],[Char])]-> [Int]-> Int -> Int
arquerosPorEquipo [] _ _ = 0
arquerosPorEquipo _ [] _ = 0
arquerosPorEquipo (arq:xs) goles totalGoles = totalGoles - sumador goles 

sumador :: [Int] -> Int
sumador [] = 0
sumador (x:xs) = x + sumador xs


{-2) Equipos Válidos
problema equiposValidos (arquerosPorEquipo: seq<String X String>): Bool { -- (nombre equipo, nombre arquero)
	requiere: {True}
	asegura: {
	(res = True) <=> arquerosPorEquipo no contiene nombres de clubes repetidos, ni arqueros repetidos, ni jugadores con nombre del club
	}
-}

-- equiposValidos :: [([Char],[Char])] -> Bool
-- equiposValidos [] = False
-- equiposValidos arquerosPorEquipo = condicionNombresRepe arquerosPorEquipo && condcionarqueros arquerosPorEquipo && condicionjugadoresclub arquerosPorEquipo

-- condicionNombresRepe :: [([Char],[Char])] -> Bool
-- condicionNombresRepe [] = True
-- condicionNombresRepe ((x,y):xs) = not (pertenece x xs) && condicionNombresRepe xs


-- condcionarqueros :: [([Char],[Char])] -> Bool
-- condcionarqueros [] = True
-- condcionarqueros ((x,y):xs) = not (pertenece y xs) && condcionarqueros xs

-- condicionjugadoresclub :: [([Char],[Char])] -> Bool
-- condicionjugadoresclub [] = True
-- condicionjugadoresclub ((x,y):xs) | x == y = False
--                                   | otherwise = condicionjugadoresclub xs && condicionjugadoresclub xs

-- pertenece :: [([Char],[Char])] -> Bool
-- pertenece _ _ [] = False
-- pertenece f n ((x, y):xs) = n == f (x, y) || pertenece f n xs

equiposValidos :: [([Char],[Char])] -> Bool
equiposValidos [] = False
equiposValidos arquerosPorEquipo = condicionNombresRepe arquerosPorEquipo && condcionArqueros arquerosPorEquipo && condicionJugadoresClub arquerosPorEquipo

condicionNombresRepe :: [([Char],[Char])] -> Bool
condicionNombresRepe [] = True
condicionNombresRepe ((x, y):xs) = not (pertenece fst x xs) && condicionNombresRepe xs

condcionArqueros :: [([Char],[Char])] -> Bool
condcionArqueros [] = True
condcionArqueros ((x, y):xs) = not (pertenece snd y xs) && condcionArqueros xs

condicionJugadoresClub :: [([Char],[Char])] -> Bool
condicionJugadoresClub [] = True
condicionJugadoresClub ((x, y):xs) = x /= y && condicionJugadoresClub xs

pertenece :: (Eq b) => ((a, a) -> b) -> b -> [(a, a)] -> Bool
pertenece _ _ [] = False
pertenece f n ((x, y):xs) = n == f (x, y) || pertenece f n xs

-- 3) Porcentaje de goles
-- problema porcentajeDeGoles (arquero: String, arquerosPorEquipo: seq<String X String>, goles: seq<Z>): R {
-- 	requiere: {La segunda componente de algún elemento de arquerosPorEquipo es arquero}
-- 	requiere: {equiposValidos(arquerosPorEquipo)}
-- 	requiere: {|arquerosPorEquipo| = |goles|}
-- 	requiere: {Todos los elementos de goles son mayores o iguales a 0}
-- 	requiere: {Hay al menos un elemento de goles mayores estricto a 0}
-- 	asegura: {
-- 	res es el porcentaje de goles que recibió arquero sobre el total de goles recibidos por arqueros titulares
-- 	}
-- }

porcentajeDeGoles :: [Char] -> [([Char],[Char])] -> [Int] -> Float
porcentajeDeGoles _ [] _ = 0.0
porcentajeDeGoles _ _ [] = 0.0
porcentajeDeGoles arquero (x:xs) goles | arquero == snd x = porcentajeTotal goles 
									   | otherwise = porcentajeDeGoles arquero xs goles

porcentajeTotal :: [Int] -> Float
porcentajeTotal [] = 0.0
porcentajeTotal goles = (fromIntegral (sumatotalGoles goles) / fromIntegral (qGoles goles)) / 10

qGoles :: [Int] -> Int
qGoles [] = 0
qGoles (x:xs) = 1 + qGoles xs

sumatotalGoles :: [Int] -> Int
sumatotalGoles [] = 0
sumatotalGoles (x:xs) = x + sumatotalGoles xs

-- 4) Valla Menos Vencida
-- problema vallaMenosVencida (arquerosPorEquipo: seq<String X String>, goles: seq<Z>): String {
-- 	requiere: {equiposValidos(arquerosPorEquipo)}
-- 	requiere: {|arquerosPorEquipo| = |goles|}
-- 	requiere: {Todos los elementos de goles son mayores o iguales a 0}
-- 	requiere: {|goles| > 0}
-- 	asegura: {
-- 	res es alguno de los arqueros de arquerosPorEquipo que menor goles recibió de acuerdo a goles
-- 	}

-- vallaMenosVencida :: [(String,String)] -> [Int] -> String
-- vallaMenosVencida [] _ = []
-- vallaMenosVencida _ [] = []
-- vallaMenosVencida (x:xs) (goles:gs) | goles < teDigoGolDeArquero(goles:gs) = (snd x) vallaMenosVencida xs (goles:gs)
-- 									| otherwise = vallaMenosVencida xs (goles:gs)

-- teDigoGolDeArquero :: [Int] -> Int
-- teDigoGolDeArquero [] = 0
-- teDigoGolDeArquero (y:z:ys) | y < z = y teDigoGolDeArquero  (z:ys)
-- 							| otherwise= teDigoGolDeArquero  (z:ys)

-- minimo :: [Int] -> Int
-- minimo [] = 0
-- minimo [x] = x
-- minimo (x:y:xs) | x > y = y : minimo (y:xs)
-- 				| otherwise= minimo (y:xs)


-- -- Encuentra el arquero que ha recibido la menor cantidad de goles
-- vallaMenosVencida :: [(String, String)] -> [Int] -> String
-- vallaMenosVencida [] _ = ""
-- vallaMenosVencida _ [] = ""
-- vallaMenosVencida arqueros goles = snd (vallaMenosVencidaAux (zip arqueros goles) ((head arqueros), head goles))

-- -- Función auxiliar para recorrer la lista y encontrar el arquero con menos goles
-- vallaMenosVencidaAux :: [((String, String), Int)] -> ((String, String), Int) -> ((String, String), Int)
-- vallaMenosVencidaAux [] minArq = minArq
-- vallaMenosVencidaAux (((eq, arq), g):xs) ((minEq, minArq), minGoles)
--   | g < minGoles = vallaMenosVencidaAux xs ((eq, arq), g)
--   | otherwise = vallaMenosVencidaAux xs ((minEq, minArq), minGoles)


