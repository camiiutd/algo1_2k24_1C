mediaMovilN :: [Int] -> Int -> Float
mediaMovilN [] _ = 0
mediaMovilN (x:xs) n | longitud (x:xs) > n = mediaMovilN xs n 
                     | longitud (x:xs) == n = promedio (sumoElem (x:xs)) n

longitud :: [Int] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs 
 
sumoElem :: [Int] -> Int
sumoElem [] = 0
sumoElem (x:xs) = x + sumoElem xs 

promedio :: Int -> Int -> Float
promedio n k = fromIntegral n / fromIntegral k

--2
esAtractivo :: Int -> Bool
esAtractivo n | 

factorizo :: Int -> Int -> [Int]
factorizo n k | mod n k == 0 = k : (factorizo (div n k) k)
              | otherwise= factorizo n 

divisores :: 