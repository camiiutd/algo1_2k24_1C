import Data.Char

esMin :: Char -> Bool
esMin a = ord a >= ord 'a' && ord a <= ord 'z'
charANat :: Char -> Int
charANat a | esMin a = (ord a) - (ord 'a')
natAChar :: Int -> Char
natAChar n | 0 <= n && n <= 25 = chr ((ord 'a' )+n)
natAChar1 :: Int -> Char
natAChar1 n | (-25) <= n && n < 0 = chr ((ord 'z' ) - ((-n - 1) `mod` 26))

{-1) Cantidad de caracteres en minúscula [2 puntos]
problema cantMinuscula (mensaje: String) : Z {
  requiere: {True}
  asegura: {res = cantidad de caracteres en minúscula en mensaje}-}

cantMinuscula :: String -> Int
cantMinuscula [] = 0
cantMinuscula (x:xs) | esMin x = 1 + cantMinuscula xs
                     | otherwise = cantMinuscula xs

{-2) Mensaje con cantidad de cambios máxima [3 puntos]
problema maximoCambios (mensajes: seq< String >) : String {
  requiere: {| mensajes | > 0}
  asegura: {res = mensaje perteneciente a mensajes tal que la
   cantidad de cambios (letras minúsculas a reemplazar) que tienen que hacerse para codificarlo es máxima. 
   En caso de haber más de un mensaje máximo, res puede ser cualquiera de ellos.}
-}
maximoCambios :: [String] -> String
maximoCambios [] = []
maximoCambios (mensajes:y:xs) | cantMinuscula mensajes >= cantMinuscula y = mensajes
                              | otherwise = maximoCambios xs

{-
3) Desplazar [2 puntos]
problema desplazar (a: Char, n: Z) : Char {
  requiere: {-25 <= n <= 25}
  asegura: {ord a >= ord 'a' ∧ ord a <= ord 'z' ∧ 0 <= n <= 25 → res es el caracter que se encuentra a n posiciones más adelante en el alfabeto (si se llega al final se comienza desde el principio)}
  asegura: {ord a >= ord 'a' ∧ ord a <= ord 'z' ∧ -25 <= n < 0 → res es el caracter que se encuentra a n posiciones más atrás en el alfabeto (si se llega al principio se comienza desde el final)}
  asegura: {¬ (ord a >= ord 'a' ∧ ord a <= ord 'z') → res = a }

Ejemplos:

desplazar 'b' 2 devuelve 'd'
desplazar 'b' -1 devuelve 'a'
desplazar 'x' 4 devuelve 'b'
desplazar 'b' -3 devuelve 'y'
desplazar ';' 2 devuelve ';'
-}-}

desplazar :: Char -> Int -> Char
desplazar a n
  | esMin a && 0 <= n && n <= 25 = natAChar (((charANat a + n) `mod` 26 + 26) `mod` 26)
  | esMin a && -25 <= n && n < 0 = natAChar1 (((charANat a - n) `mod` 26 + 26) `mod` 26)
  | otherwise = a


{-4) Codificar mensaje [2 puntos]
problema codificar (mensaje: String, n: Z) : String {
  requiere: {0 <= n <= 25}
  asegura: {res = versión codificada del mensaje, donde cada caracter en minúscula del mensaje se sustituye por la letra minúscula 
  que se encuentra n posiciones más adelante en el alfabeto. Los caracteres que no son minúscula no se sustituyen.}
}-}


codificar :: String -> Int -> String
codificar (x:xs) 0 = (x:xs)
codificar [] _ = []
codificar (x:xs) n | esMin x = desplazar x n : codificar xs n
                   | otherwise = x : codificar xs n

{-5) Decodificar mensaje [1 puntos]
problema decodificar (mensaje: String, n: Z) : String {
  requiere: {0 <= n <= 25}
  asegura: {res = versión decodificada del mensaje, donde cada caracter en minúscula del mensaje se sustituye por la letra minúscula que se encuentra n posiciones más atrás en el alfabeto. 
  Los caracteres que no son minúscula no se sustituyen.}
-}

decodificar :: String -> Int -> String
decodificar (x:xs) 0 = (x:xs)
decodificar [] _ = []
decodificar (x:xs) n | esMin x = desplazar x (-n) : decodificar xs n
                     | otherwise = decodificar xs n