-- parcial de 2023 fin d año
-- 1
votosEnBlanco :: [([Char],[Char])] -> [Int] -> Int -> Int
votosEnBlanco [] _ _ = 0
votosEnBlanco _ [] _ = 0
votosEnBlanco formulas votos n = sumatotalVotos votos - n 

sumatotalVotos :: [Int] -> Int
sumatotalVotos [] = 0
sumatotalVotos (x:xs) = x + sumatotalVotos xs

-- 2
formulasValidas :: [([Char],[Char])] -> Bool
formulasValidas [] = True
formulasValidas [x] = True
formulasValidas ((a,b):xs) | a == b = False
                           | perteneceTupla a xs = False
                           | perteneceTupla b xs =False
                           | otherwise = formulasValidas xs

perteneceTupla :: [Char] -> [([Char],[Char])] -> Bool
perteneceTupla _ [] = False
perteneceTupla string ((a,b):xs) | string == a = True
                                 | string == b = True
                                 | otherwise = perteneceTupla string xs

--3
porcentajeDeVotos :: [Char] -> [([Char],[Char])] -> [Int] -> Float
porcentajeDeVotos _ [] _ = 0.0
porcentajeDeVotos _ _ [] = 0.0
porcentajeDeVotos presi formulas votos = sacaPromedio (encuentraPresiVoto presi formulas votos) (qVotos votos)

encuentraPresiVoto :: [Char] -> [([Char],[Char])] -> [Int] -> [([Char],Int)]
encuentraPresiVoto _ [] _ = []
encuentraPresiVoto _ _ [] = []
encuentraPresiVoto presi ((a,b):xs) (n:ns) | presi == a = (presi,n) : encuentraPresiVoto presi xs ns
                                           | otherwise = encuentraPresiVoto presi xs ns
                                          
qVotos :: [Int] -> Int
qVotos [] = 0
qVotos (x:xs) = 1 +qVotos xs

sacaPromedio :: [([Char],Int)] -> Int -> Float
sacaPromedio [] _ = 0.0
sacaPromedio ((a,b):xs) n = (fromIntegral b) / (fromIntegral n)

--4
proximoPresidente :: [([Char],[Char])] -> [Int] -> [Char]
proximoPresidente [] _ = ""
proximoPresidente _ [] = ""
proximoPresidente formula votos = fst (maximo (proxPresiAux formula votos))

proxPresiAux :: [([Char],[Char])] -> [Int] -> [([Char],Int)]
proxPresiAux [] _ = []
proxPresiAux _ [] = []
proxPresiAux ((a,b):xs) (n:ns) = (a,n) : proxPresiAux xs ns

maximo :: [([Char],Int)] -> ([Char],Int)
maximo [] = ("", 0)
maximo [(a,b)] = (a,b)
maximo ((a,b):(c,d):xs) | b > d = maximo ((a,b):xs)
                        | otherwise = maximo ((c,d):xs)


