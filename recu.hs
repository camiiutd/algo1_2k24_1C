import Data.Char

esMin :: Char -> Bool
esMin a = ord a >= ord 'a' && ord a <= ord 'z'
charANat :: Char -> Int
charANat a | esMin a = (ord a) - (ord 'a')
natAChar :: Int -> Char
natAChar n | 0 <= n && n <= 25 = chr ((ord 'a' )+n)
natAChar1 :: Int -> Char
natAChar1 n | (-25) <= n && n < 0 = chr ((ord 'z' )+n)

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
desplazar a n | a == natAChar n = a
              | a == natAChar1 n = a
              | otherwise = desplazarAux a n

desplazarAux :: Char -> Int -> Char
desplazarAux x y | charANat x == y = x 
                 | otherwise = desplazarAux x (y-1)