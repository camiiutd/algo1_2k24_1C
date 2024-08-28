import Distribution.Simple.Setup (falseArg)
import Distribution.Simple.Utils (xargs)
duplicar :: Int -> Int
duplicar x = 2*x

--b
raizCuadrada :: Int -> Float
raizCuadrada x = sqrt(fromIntegral x)
--c
enteroMasCercanoPositivo :: Float->Int
enteroMasCercanoPositivo x | x < 0 || x< 0.5 = 0
                           | x<1= 1
                           | otherwise = 1 + enteroMasCercanoPositivo (x-1)
                           

--d
raicesCuadradasUno:: [Int]->[Float]
raicesCuadradasUno [] = []
raicesCuadradasUno (x:xs) = raizCuadrada x : raicesCuadradasUno xs

--g
raicesCuadradasCuatro :: [Int] -> [Float]
raicesCuadradasCuatro [] = []
raicesCuadradasCuatro (x:xs) | x > 0 = raizCuadrada x : raicesCuadradasCuatro xs 
                             | otherwise= (fromIntegral x) : raicesCuadradasCuatro xs


--
duplicarTodos :: [Int] -> [Int]
duplicarTodos [] = []
duplicarTodos (x:xs) = 2*x : duplicarTodos xs

--
ordenar :: (Ord t, Eq t) => [t] -> [t]
ordenar [] = []
ordenar (x:xs) = insertar x (ordenar xs) 

insertar :: (Ord t) => t -> [t] -> [t]
insertar x [] = [x]
insertar x (y:ys) | x <= y = x:y:ys
                  | otherwise= y : insertar x ys

quitarRep :: Int -> [Int] -> [Int]
quitarRep _ []= []
quitarRep x (y:ys) | x == y = quitarRep x ys 
                   | otherwise= y : quitarRep x ys 


ordenoDos :: [Int] -> [Int]
ordenoDos [] = []
ordenoDos xs | head xs > head(tail xs) = tail xs ++ head xs : []
                 | otherwise= xs
 
long :: [Int] -> Int
long [] = 0
long (x:xs) = 1 + long xs 

--
pares1 :: [Int] -> [Int]
pares1 [] = []
pares1 (x:xs) | mod x 2 == 0= x : pares1 xs
              | otherwise= pares1 xs
--
sumarAbsMayorA5 :: [Int] -> Int
sumarAbsMayorA5 [] = 0


sumarAbsAUX :: Int -> Int
sumarAbsAUX x| x < 0 = 1 
             | x >= 0 = x