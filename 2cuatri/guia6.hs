--SISTEMA DE STOCK--
--1
generarStock :: [String] -> [(String,Int)]
generarStock [] =[]
generarStock (x:xs) = (x,contadorStock (x:xs) x) : generarStock (eliminoRepe xs x)

contadorStock :: [String] -> String -> Int
contadorStock [] _= 0
contadorStock (x:xs) n | n == x = 1 + contadorStock xs n 
                       | otherwise= contadorStock xs n 

eliminoRepe :: [String] -> String ->[String] 
eliminoRepe [] _ = []
eliminoRepe (x:xs) n | n ==x = eliminoRepe xs n 
                     | otherwise= x :( eliminoRepe xs n )

--2
stockDeProducto :: [(String,Int)] -> String -> Int
stockDeProducto [] _ = 0
stockDeProducto (x:xs) n | pertenece (x:xs) n == False = 0
                         | (fst x) ==n = snd x
                         | otherwise= stockDeProducto xs n 

pertenece :: [(String,Int)]  -> String -> Bool
pertenece [] _ = False
pertenece (x:xs) n | fst x == n = True
                   | otherwise= pertenece xs n 

-- [("hola",3),("aa",2),("eeee",1),("ee",1)] [("hola",4.0),("aa",2.0),("eeee",10.0),("ee",3.0)] 
-- [("hola",3),("ee",1),("aa",2),("eeee",1)] [("eeee",10.0),("hola",4.0),("ee",3.0),("aa",2.0)] 
--3
dineroEnStock :: [(String,Int)] -> [(String,Float)] -> Float
dineroEnStock [] [] = 0
dineroEnStock s p = sumotodo s p

sumotodo :: [(String,Int)] -> [(String,Float)] -> Float
sumotodo [] _ = 0
sumotodo _ [] = 0
sumotodo (x:xs) (y:ys) | perteneceTupla x (y:ys) =  fromIntegral(snd x )* (snd(perteneceTupla2 x (y:ys)))  + sumotodo xs (y:ys)


perteneceTupla :: (String,Int) -> [(String,Float)] ->  Bool
perteneceTupla _ [] =False
perteneceTupla x (y:ys) | fst x == fst y = True
                        | otherwise= perteneceTupla x ys

perteneceTupla2 :: (String,Int) -> [(String,Float)] ->  (String,Float)
perteneceTupla2 x (y:ys) | fst x == fst y = y
                         | otherwise= perteneceTupla2 x ys

--4
aplicarOferta :: [(String,Int)] -> [(String, Float)]-> [(String,Float)]
aplicarOferta [] _ = []
aplicarOferta _ [] = []
aplicarOferta (x:xs) (y:ys) | snd x > 10= (fst x, (perteneceTupla2 x *0.80) : aplicarOferta xs (y:ys)
                            | snd x <=10 = (fst x, perteneceTupla2 (snd y)) : aplicarOferta xs (y:ys)
