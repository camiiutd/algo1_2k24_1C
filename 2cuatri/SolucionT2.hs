module SolucionT2 where
  
relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x:xs) | fst x == snd x= False
                         | pertenece xs x = False
                         | pertenece xs (reverseTupla x) = False
                         | otherwise= relacionesValidas xs

reverseTupla :: (String, String)->(String, String)
reverseTupla (a,b) = (b,a)

pertenece :: [(String, String)] -> (String, String) -> Bool
pertenece [] _ = False
pertenece (x:xs) n | x == n = True
                   | otherwise= pertenece xs n 

--

personas :: [(String, String)] -> [String]
personas [] = []
personas x =  sacolosrepe (auxpersonas x)
 
auxpersonas::  [(String, String)] -> [String]
auxpersonas [] = []
auxpersonas (x:xs) = fst x : snd x : auxpersonas xs


pertenecestring :: [String] ->String -> Bool
pertenecestring [] _ = False
pertenecestring (x:xs) n | n ==x = True
                         | otherwise= pertenecestring xs n 

sacoTodos :: [String] -> String -> [String] 
sacoTodos [] _ = [] 
sacoTodos (x:xs) m 
                   | m == x = sacoTodos xs m
                   | otherwise=x: sacoTodos xs m

sacolosrepe :: [String] -> [String]
sacolosrepe [] = []
sacolosrepe (x:xs) | pertenecestring xs x = x : sacolosrepe (sacoTodos xs x) 
                   |otherwise= x: sacolosrepe xs 

 
--

amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] =[]
amigosDe n (x:xs) | n == fst x = (snd x) : amigosDe n xs
                  | n ==snd x = (fst x ) : amigosDe n xs
                  | otherwise= amigosDe n xs

eliminotuplasrep :: String -> [(String, String)] -> [(String, String)]
eliminotuplasrep _ [] = []
eliminotuplasrep n (x:xs)  | n == fst x || n == snd x = xs
                           | otherwise= x : eliminotuplasrep n xs 

--

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos x = maximo (auxpersonas x)

maximo :: [String] -> String
maximo [x] = x
maximo (x:xs) | contador x xs >= auxiliarmx xs = x
              | otherwise= maximo xs

auxiliarmx :: [String] -> Int
auxiliarmx [] = 0
auxiliarmx (x:xs) | contador x xs >= auxiliarmx xs = contador x xs
                  | otherwise= auxiliarmx xs


contador :: String -> [String] -> Int
contador _ [] = 0
contador x (y:ys)  | x == y = 1+contador x ys
                   |otherwise= contador x ys