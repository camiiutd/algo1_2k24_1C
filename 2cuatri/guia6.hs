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
-- [("hola",3),("ee",1),("aa",20),("eeee",1)] [("eeee",10.0),("hola",4.0),("ee",3.0),("aa",2.0)] 
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
aplicarOferta (x:xs) (y:ys) | snd x > 10= (fst x, (snd ((perteneceTupla2 x  (y:ys))))*0.80) : aplicarOferta xs (y:ys)
                            | snd x <=10 = (fst x, (snd(perteneceTupla2 x  (y:ys)))) : aplicarOferta xs (y:ys)

--SOPA DE NÙMEROS--

--5
maximo :: [[Int]] -> Int
maximo [] = 0
maximo (x:xs) | maximoaux x >= maximo xs = maximoaux x 
              | otherwise= maximo xs 

maximoaux :: [Int] ->Int
maximoaux [] = 0
maximoaux [x]=x
maximoaux (x:xs) | x >= maximoaux xs = x 
                 | otherwise= maximoaux xs

-- [[1,2,4],[3,2,],[4,3,6],[1,4,2]]

--6
masRepetido :: [[Int]] ->Int
masRepetido [] = 0
masRepetido x = juntoTodito (aplanar x)

aplanar :: [[Int]] -> [Int] 
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

contador :: [Int] -> Int -> Int
contador [] _ =0
contador (x:xs) n | n == x = 1 + contador xs n
                  | otherwise= contador xs n

eliminorepetidos :: [Int] -> Int -> [Int]
eliminorepetidos [] _ = []
eliminorepetidos (x:xs) n | n == x = xs
                          | otherwise= x : (eliminorepetidos xs n)

juntoTodito :: [Int] ->Int
juntoTodito [] =0
juntoTodito [x] =x
juntoTodito (x:xs) | contador (x:xs) x >= contador xs (juntoTodito xs) = x
                   | otherwise= juntoTodito (eliminorepetidos (x:xs) x)

--7
valoresDeCamino :: [[Int]] ->[(Int,Int)] -> [Int]
valoresDeCamino [] _ =[]
valoresDeCamino _ [] = []
valoresDeCamino x ((a,b):xs) = [(agarroLaColumna (voyBorrandoFilas x (a,b)) (a,b))] ++ valoresDeCamino x xs 

voyBorrandoFilas :: [[Int]] ->(Int,Int) -> [Int]
voyBorrandoFilas [] _ = []
voyBorrandoFilas [x] _ = x
voyBorrandoFilas (x:xs) (a,b) | a ==1= x
                              | otherwise= voyBorrandoFilas xs ((a-1),b)

agarroLaColumna :: [Int] -> (Int,Int) -> Int
agarroLaColumna [] _ = 0
agarroLaColumna (x:xs) (a,b) | b == 1= x
                             | otherwise= agarroLaColumna xs (a,(b-1))

--8
esCaminoFibo :: [Int] -> Int -> Bool
esCaminoFibo [] _ = True
esCaminoFibo (x:xs) n | fibonacci n == x = esCaminoFibo xs (n+1) 
                      |otherwise= False

fibonacci :: Int -> Int
fibonacci 0=0
fibonacci 1=1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--PERFECTOS AMIGOS--
--9
divisoresPropios:: Int -> [Int]
divisoresPropios n = divisores n 1

divisores :: Int -> Int ->[Int]
divisores n k| k == n = []
             | mod n k==0= k : divisores n (k+1)
             | otherwise= divisores n (k+1)

--10
sonAmigos :: Int ->Int->Bool
sonAmigos n m = sumoLista (divisoresPropios n) == m && sumoLista (divisoresPropios m ) == n

sumoLista :: [Int] ->Int
sumoLista [] =0
sumoLista (x:xs) =x + sumoLista xs 

--11
losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos n = perfectos n 1

perfectos :: Int -> Int -> [Int]
perfectos n k  | n ==0=[]
               | sumoLista (divisoresPropios k) == k = k : perfectos (n-1) (k+1) 
               | otherwise=perfectos n (k+1)

--12
listaDeAmigos :: [Int] -> [(Int,Int)]
listaDeAmigos [] = [] 
listaDeAmigos (x:xs) | perteneceLista xs (sumoLista (divisoresPropios x) ) = (x,(sumoLista (divisoresPropios x) ) ) : (listaDeAmigos (eliminoEle xs x)  )               
                     | otherwise= listaDeAmigos (eliminoEle xs x)

eliminoEle :: [Int] -> Int-> [Int]
eliminoEle [] _ = []
eliminoEle (x:xs) m | m == x= eliminoEle xs m 
                    | otherwise= x : eliminoEle xs m 

perteneceLista :: [Int] -> Int -> Bool
perteneceLista [] _ = False
perteneceLista (x:xs) n | n ==x = True
                        |otherwise=perteneceLista xs n 
