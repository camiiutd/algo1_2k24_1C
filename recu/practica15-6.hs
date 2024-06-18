pertenece :: [[Char]] -> Char -> Bool
pertenece [] _ = False
pertenece ((x:xs):xss) a | x == a = True
                         | otherwise = pertenece (xs:xss) a

{-Ejercicio 1 (2 puntos)
problema generarStock (productos: seq⟨seq⟨Char⟩⟩): seq⟨seq⟨Char⟩ x Z⟩ {
  requiere: {True}
  asegura: {La longitud de res es igual a la cantidad de productos distintos que hay en productos}
  asegura: {Para cada producto que pertenece a productos existe un i tal que 0 <= i < |res| y res[i]0=producto y res[i]1 es igual a la cantidad de veces que aparece producto en productos}
-}
eliminar :: (Eq t) => t -> [t] -> [t]
eliminar _ [] =[]
eliminar x (y:ys) | x == y = eliminar x ys
                  | otherwise = y:(eliminar x ys)

--eliminar2 :: (Eq t) => t -> [t] -> [t]
--eliminar2 _ []=[]
--eliminar2 z (y:ys) | z == y = eliminar z ys
--                   | otherwise=eliminar2 y:(eliminar z ys)

qApariciones :: (Eq t) => t -> [t] -> Int
qApariciones _ [] = 0
qApariciones x (y:ys) | x == y = 1+qApariciones x ys
                      | otherwise = qApariciones x ys

generarStock :: [[Char]] -> [([Char],Int)]
generarStock [] = []
generarStock (x:xs) = (x, cant):generarStock (eliminar x xs)
     where cant = 1 + qApariciones x xs

{-problema stockDeProducto (stock:seq⟨seq⟨Char⟩ x Z⟩, producto: seq⟨Char⟩) : Z {
  requiere: {No hay productos repetidos en stock}
  requiere: {Todas las cantidades de los productos que hay en stock son mayores a cero}
  asegura: {(res = 0 y producto no se encuentra en el stock) o (existe un i tal que 0 <= i < |stock| y producto=stock[i]0 y res = stock[i]1)}
}-}

stockDeProducto :: [([Char],Int)] -> [Char] -> Int
stockDeProducto [] _ = 0
stockDeProducto (x:xs) y | y == fst x = snd x
                         | otherwise= stockDeProducto xs y

{-problema dineroEnStock (stock:seq⟨seq⟨Char⟩ x Z⟩, precios: seq⟨seq⟨Char⟩ x R⟩: R {
  requiere: {No hay productos repetidos en stock}
  requiere: {Todas las cantidades de los productos que hay en stock son mayores a cero}
  requiere: {No hay productos repetidos en precios}
  requiere: {Todos los precios de los productos son mayores a cero}
  requiere: {Todo producto en stock aparece en la lista de precios}
  asegura: {res es igual a la suma de los precios de todos los productos que están en stock multiplicado por la cantidad de cada producto que hay en stock}
}
Para resolver este ejercicio pueden utilizar la función del Preludio de Haskell fromIntegral que dado un valor de tipo Int devuelve su equivalente de tipo Float.-}

dineroEnStock :: [([Char],Int)] -> [([Char], Float)] -> Float
dineroEnStock [] [] = 0.0
dineroEnStock (x:xs) (y:ys) | fst x == fst y = fromIntegral(snd x) * snd y 
                            | otherwise= dineroEnStock xs ys

{-Ejercicio 4 (3 puntos)
problema aplicarOferta (stock:seq⟨seq⟨Char⟩ x Z⟩, precios: seq⟨seq⟨Char⟩ x R⟩: seq⟨seq⟨Char⟩ x R⟩ {
  requiere: {No hay productos repetidos en stock}
  requiere: {Todas las cantidades de los productos que hay en stock son mayores a cero}
  requiere: {No hay productos repetidos en precios}
  requiere: {Todos los precios de los productos son mayores a cero}
  requiere: {Todo producto en stock aparece en la lista de precios}
  asegura: {|res| = | precios|}
  asegura: {Para todo 0 <= i < |precios| si stockDeProducto(stock, precios[i]0) > 10 entonces res[i]0 = precios[i]0 y res[i]1 = precios[i]1 * 0,80}
  asegura: {Para todo 0 <= i < |precios| si stockDeProducto(stock, precios[i]0) <= 10 entonces res[i]0 = precios[i]0 y res[i]1 = precios[i]1}
}-}

condicion :: (Eq t, Ord t, Num t) => [(a, t)] -> Bool
condicion [] = False
condicion ((x,y):xs) | y>10= True
                     | otherwise= condicion xs

aplicarOferta :: [([Char],Int)] -> [([Char],Float)] -> [([Char],Float)]
aplicarOferta [] [] = []
aplicarOferta _ []=[]
aplicarOferta (x:xs) (y:ys) | condicion (x:xs) ==True= (fst y, snd y * 0.80) :aplicarOferta (x:xs) ys
                            | condicion (x:xs)==False= (fst y, snd y) : aplicarOferta (x:xs) ys
                            | otherwise= aplicarOferta xs ys

