{-Ejercicio 21. Especificar e implementar una funci´on pitagoras :: Integer ->Integer ->Integer ->Integer que dados
m, n , r ∈ N≥0, cuente cu´antos pares (p, q) con 0 ≤ p ≤ m y 0 ≤ q ≤ n satisfacen que p
2 + q
2 ≤ r
2
. Por ejemplo:
pitagoras 3 4 5 ⇝ 20
pitagoras 3 4 2 ⇝ 6-}

esMenorPitagoriano :: Integer->Integer->Integer->Bool
esMenorPitagoriano p q r = p^2 + q^2 <= r^2

pitagorasNFijo :: Integer -> Integer -> Integer -> Integer
pitagorasNFijo n m r | m < 0 = 0
                     | esMenorPitagoriano n m r = 1 + pitagorasNFijo n (m-1) r
                     | otherwise = 0 + pitagorasNFijo n (m-1) r


pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras n m r | n==0 = pitagorasNFijo 0 m r
                | otherwise = pitagorasNFijo n m r + pitagoras (n-1) m r

{-Ejercicio 20. Especificar e implementar la funci´on
 tomaValorMax :: Int ->Int ->Int que dado un n´umero entero n1 ≥ 1
y un n2 ≥ n1 devuelve alg´un m entre n1 y n2 tal que 
sumaDivisores(m) = m´ax{sumaDivisores(i) | n1 ≤ i ≤ n2}-}

{-nCondicion :: Int -> Int -> Bool
nCondicion n1 n2 = n2 >= n1 

sonDivisoresCond :: Int -> Int -> Bool
sonDivisoresCond x y = mod x y == 0

sonDivisores :: Int -> Int -> Int
sonDivisores 0 0 = 0
sonDivisores x y | mod x y == 0 = div x y + sonDivisores x y
                 | otherwise = sonDivisores (x-1) (y-1)
sumaDivisores :: Int -> Int -> Int
sumaDivisores 0 0 = 0
sumaDivisores x y | nCondicion x y == True && sonDivisoresCond x y = sonDivisores x y
                  | otherwise = sumaDivisores (x - 1) (y - 1)

tomaValorMax :: Int -> Int -> Int
tomaValorMax n m  = sumaDivisores n m-}

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n 1 = 1
sumaDivisoresHasta n i | mod n 1 == 0 = i + sumaDivisoresHasta n (i-1)
                       | otherwise = sumaDivisoresHasta n (i-1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

tomaValorMax :: Integer -> Integer
tomaValorMax 1 = 1
tomaValorMax n = max  (sumaDivisores n) (tomaValorMax (n-1))

valorMax :: Integer -> Integer -> Integer
valorMax n1 n2 | n1/=n2 = max (sumaDivisores n1) (valorMax (n1+1) n2)
               | otherwise = sumaDivisores n1 

--
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorHasta n 2

menorDivisorHasta :: Int -> Int -> Int 
menorDivisorHasta n q | mod n q == 0 = q
                      | otherwise = menorDivisorHasta n (q+1)

esPrimo :: Int ->Bool
esPrimo n | menorDivisor n == n = True
          | otherwise = False

esPrimoLista :: [Int] -> [Int]
esPrimoLista [] = []
esPrimoLista (x:xs) | esPrimo x = x : esPrimoLista xs
                    | otherwise = esPrimoLista xs

---------------------a ver el recu 2023 2ndo cuatri :/
{-En el último debate presidencial cada equipo de campaña quería comunicarse con su candidato por teléfono sin que el otro
 equipo pudiera interceptar y leer el mensaje. Como no tuvieron mucho tiempo desde las elecciones decidieron implementar 
 en Haskell el código de César, que es una de las técnicas de cifrado más simples. Consiste en una codificación por
  sustitución en el que una letra en el texto original es reemplazada por otra letra que se encuentra un número fijo
   de posiciones más adelante en el alfabeto. Para mayor simplicidad, codificaron solo las letras minúsculas del mensaje y eliminaron la "ñ" del conjunto de letras minúsculas.
    Todos los restantes símbolos quedarían inalterados en el mensaje codificado

1) Cantidad de caracteres en minúscula [2 puntos]
problema cantMinuscula (mensaje: String) : Z {
  requiere: {True}
  asegura: {res = cantidad de caracteres en minúscula en mensaje}
}
2) Mensaje con cantidad de cambios máxima [3 puntos]
problema maximoCambios (mensajes: seq< String >) : String {
  requiere: {| mensajes | > 0}
  asegura: {res = mensaje perteneciente a mensajes tal que la cantidad de cambios (letras minúsculas a reemplazar) que tienen que hacerse para codificarlo es máxima. En caso de haber más de un mensaje máximo, res puede ser cualquiera de ellos.}
}
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


4) Codificar mensaje [2 puntos]
problema codificar (mensaje: String, n: Z) : String {
  requiere: {0 <= n <= 25}
  asegura: {res = versión codificada del mensaje, donde cada caracter en minúscula del mensaje se sustituye por la letra minúscula que se encuentra n posiciones más adelante en el alfabeto. Los caracteres que no son minúscula no se sustituyen.}
}
5) Decodificar mensaje [1 puntos]
problema decodificar (mensaje: String, n: Z) : String {
  requiere: {0 <= n <= 25}
  asegura: {res = versión decodificada del mensaje, donde cada caracter en minúscula del mensaje se sustituye por la letra minúscula que se encuentra n posiciones más atrás en el alfabeto. Los caracteres que no son minúscula no se sustituyen.}
-}
--}


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

--cantMinuscula :: String -> Int
