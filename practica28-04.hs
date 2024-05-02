
--problema relacionesValidas (relaciones: seq⟨String × String⟩) : Bool {
--requiere: {True}
--asegura: {(res = true) ↔ no hay tuplas en relaciones con ambas componentes iguales ni tuplas repetidas (sin considerar
--el orden)}
--}
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

relacionesValidas :: [(String,String)] -> Bool
relacionesValidas [] = True 
relacionesValidas ((a,b):xs) | pertenece (a,b) xs = False
                             | pertenece (b,a) xs = False
                             | a == b = False
                             | otherwise = relacionesValidas xs 

-------------------
--problema personas (relaciones: seq⟨String × String⟩) : seq⟨String⟩ {
--requiere: {relacionesV alidas(relaciones)}
--asegura: {resu tiene exactamente los elementos que figuran en alguna tupla de relaciones en cualquiera de las dos
--posiciones, sin repetir}
--}

personas :: [(String, String)] -> [String]
personas [] = []
personas ((a,b):xs) | pertenece a (personas xs) && pertenece b (personas xs) = personas xs
                    | pertenece a (personas xs) = b:personas xs  
                    | pertenece b (personas xs) = a:personas xs  
                    | otherwise = a:b:(personas xs)

-------------------
--problema amigosDe (persona: String, relaciones: seq⟨String × String⟩) : seq⟨String⟩ {
--requiere: {relacionesV alidas(relaciones)}
--asegura: {resu tiene exactamente los elementos que figuran en alguna tupla de relaciones en las que alguna de las
--componentes es persona}
--}

amigosDe :: String -> [(String,String)] -> [String]
amigosDe _ [] = []
amigosDe y ((a,b):xs) | y == a = b : personas xs
                      | y == b = a : personas xs
                      | otherwise = amigosDe y xs
-------------------
--problema personaConMasAmigos (relaciones: seq⟨String × String⟩) : String {
--requiere: {relaciones no vac´ıa}
--requiere: {relacionesV alidas(relaciones)}
--asegura: {resu es el Strings que aparece m´as veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
--}

longitud :: String -> [(String, String)] -> Int
longitud a b = length (amigosDe a b)

mayorAmigueroAux :: [String]->[(String,String)]->String
mayorAmigueroAux [x] _ = x
mayorAmigueroAux (x:y:xs) relaciones | longitud x relaciones >= longitud y relaciones = mayorAmigueroAux (x:xs) relaciones
                                     | otherwise = mayorAmigueroAux (y:xs) relaciones

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos [] = []
personaConMasAmigos x = mayorAmigueroAux (personas x) x

------1.1
-- https://gitlab.com/faustomartinez/uba-algoritmos-y-estructuras-de-datos-i/-/blob/main/1er-parcial/PrimerParcial.pdf

cantidadVotos :: [Int] -> Int
cantidadVotos [] = 0
cantidadVotos (x:xs) = x + cantidadVotos xs

votosEnBlanco :: [(String,String)] -> [Int] -> Int -> Int
votosEnBlanco (x:xs) y z = z - (cantidadVotos y)

---1.2
perteneceTupla :: String -> [(String, String)] -> Bool
perteneceTupla x [] = False
perteneceTupla x ((a,b):xs) | x == a = True 
                            | x == b = True 
                            | otherwise = perteneceTupla x xs

formulasValidas :: [(String, String)] -> Bool
formulasValidas [] = True
formulasValidas ((a,b):xs) | a == b = False
                           | perteneceTupla a xs = False
                           | perteneceTupla b xs = False
                           | otherwise = formulasValidas xs


---1.3
--problema porcentajeDeVotos(presidente : String, f ormulas : seq < String × String >, votos : seq < Z >) : R{
--requiere : {La primera componente de algun elemento de f ormulas es presidente}
--requiere : {f ormulasV alidas(f ormulas)}
--requiere : {|f ormulas| = |votos|}
--requiere : { Todos los elementos de votos son mayores o iguales que 0}
--requiere : { Hay al menos un elemento de votos que es mayor estricto que 0}
--asegura : {res es el porcentaje de votos que obtuvo la f´ormula encabezada por presidente sobre el total de votos afirmativos }
--}

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

porcentajeDeVotos :: String -> [(String,String)] -> Int -> Float
porcentajeDeVotos _ [] _ = 0
porcentajeDeVotos presi ((presidente,vice):xs) votos | presi == presidente = pasajevotos presi ((presidente,vice):xs) votos
                                                     | otherwise = porcentajeDeVotos presi xs votos 


pasajevotos :: String -> [(String,String)] -> Int -> Float
pasajevotos presi ((a,b):xs) votos  | presi == a = division votos 100
                                | otherwise = pasajevotos presi xs votos

--------
--problema proximoPresidente(f ormulas : seq < String × String >, votos : seq < Z >) : String{
--requiere : {La primera componente de algun elemento de f ormulas es presidente}
--requiere : {f ormulasV alidas(f ormulas)}
--requiere : {|f ormulas| = |votos|}
--requiere : { Todos los elementos de votos son mayores o iguales que 0}
--requiere : { Hay al menos un elemento de votos que es mayor estricto que 0}
--requiere : {|formulas| > 0}
--asegura : {res es el candidato a presidente de formulas m´as votado de acuerdo a los votos contabilizados en votos}
--}

cantidadDeVotos :: String -> [(String,String)] -> [Int] -> Int
cantidadDeVotos _ [] _ = 0
cantidadDeVotos presidente ((candidato,vice):xs) (votos:ys)
    | presidente == candidato = votos
    | otherwise = cantidadDeVotos presidente xs ys


proximoPresidenteAux :: [(String,String)] -> [(String,String)] -> [Int] -> String
proximoPresidenteAux ((candidato,vice):[]) _ _ = candidato
proximoPresidenteAux ((candidato1,vice1):(candidato2,vice2):xs) formulas votos
    | votosCandidato1 >= votosCandidato2 = proximoPresidenteAux ((candidato1,vice1):xs) formulas votos
    | otherwise = proximoPresidenteAux ((candidato2,vice2):xs) formulas votos
    where
        votosCandidato1 = cantidadDeVotos candidato1 formulas votos
        votosCandidato2 = cantidadDeVotos candidato2 formulas votos

proximoPresidente :: [(String,String)] -> [Int] -> String
proximoPresidente formulas votos = proximoPresidenteAux formulas formulas votos
