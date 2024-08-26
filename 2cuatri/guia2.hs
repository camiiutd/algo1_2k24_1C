duplicar :: Int -> Int
duplicar x = 2*x

--b
raizCuadrada :: Int -> Float
raizCuadrada x = sqrt(fromIntegral x)
--c
-- enteroMasCercanoPositivo :: Float->Int
-- enteroMasCercanoPositivo x = x / 10

--d
raicesCuadradasUno:: [Int]->[Float]
raicesCuadradasUno [] = []
raicesCuadradasUno (x:xs) = raizCuadrada x : raicesCuadradasUno xs

--g
raicesCuadradasCuatro :: [Int] -> [Float]
raicesCuadradasCuatro [] = []
raicesCuadradasCuatro (x:xs) | x > 0 = raizCuadrada x : raicesCuadradasCuatro xs 
                             | otherwise= (fromIntegral x) : raicesCuadradasCuatro xs

--h
-- raicesCuadradasCinco :: [Int] -> [Float]
-- raicesCuadradasCinco [] = []
-- raicesCuadradasCinco (x:xs) | 

-- sumoPos :: [Int] -> Int
-- sumoPos [] = 0
-- sumoPos (x:xs)= 1 + sumoPos xs

--ej 5
cantidadColectivosLinea 
