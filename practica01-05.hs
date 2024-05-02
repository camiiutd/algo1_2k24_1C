{-Ejercicio 18. Implementar una funci´on mayorDigitoPar :: Integer ->Integer seg´un la siguiente especificaci´on:
problema mayorDigitoPar (n: N) : N {
requiere: { T rue }
asegura: { resultado es el mayor de los d´ıgitos pares de n. Si n no tiene ning´un d´ıgito par, entonces resultado es -1.
-}

mayorDigitoPar :: Int -> Int
mayorDigitoPar n = mayorDigitoParAux n (-1)

mayorDigitoParAux :: Int -> Int -> Int
mayorDigitoParAux n x | n == 0 = x
                      | esPar (digito n) && digito n > x = mayorDigitoParAux (sacarUltimo n) (digito n)
                      | otherwise = mayorDigitoParAux (sacarUltimo n) x 

esPar :: Int -> Bool
esPar n = mod n 2 == 0

sacarUltimo :: Int -> Int
sacarUltimo n = div n 10 

digito :: Int -> Int
digito n = mod n 10

----- https://wiki.haskell.org/99_questions/Solutions
{-Problem 1
(*) Find the last element of a list.Solutions
 
(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z'-}

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = last xs

--
{-Problem 3
(*) Find the K'th element of a list.Solutions
 
The first element in the list is number 1. Example:

* (element-at '(a b c d e) 3)
c
Example in Haskell:

λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'
-}

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)
-- consultar por lista vacía en este caso

--
{-Problem 4
(*) Find the number of elements in a list.Solutions
 
Example in Haskell:

λ> myLength [123, 456, 789]
3
λ> myLength "Hello, world!"
13-}

myLength:: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--
{-Problem 5
(*) Reverse a list.Solutions
 
Example in Haskell:

λ> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
λ> myReverse [1,2,3,4]
[4,3,2,1]-}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-----
{-Problem 6
(*) Find out whether a list is a palindrome.Solutions
 
Hint: A palindrome can be read forward or backward; e.g. (x a m a x). osea capicua

Example in Haskell:

λ> isPalindrome [1,2,3]
False
λ> isPalindrome "madamimadam"
True
λ> isPalindrome [1,2,4,8,16,8,4,2,1]
True-}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) | myReverse (x:xs) == (x:xs) = True
                    | otherwise = False

----
{-Problem 7
(**) Flatten a nested list structure.
 
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:

* (my-flatten '(a (b (c d) e)))
(A B C D E)
Example in Haskell:
λ> flatten (Elem 5)
[5]
λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
λ> flatten (List [])
[]-}

flatten :: [[t]] -> [t]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

--
{-Problem 8
(**) Eliminate consecutive duplicates of list elements.Solutions
 
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:

* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)
Example in Haskell:

λ> compress "aaaabccaadeeee"
"abcade"-}

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) | x == y = compress (y:xs)
                  | otherwise = x : compress (y:xs)

----------
{-Problem 9 PREGUNTAR
(**) Pack consecutive duplicates of list elements into sublists.Solutions
 
If a list contains repeated elements they should be placed in separate sublists.

Example:

* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))
Example in Haskell:

λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]-}

pack :: Eq a => [a] -> [a]
pack [] = []
pack (x:xs) = packaux x xs

packaux :: Eq a => [a] -> a -> [a]
packaux  (x:xs) y | x == y = x ++ xs
                  | otherwise = packaux xs y

--------
{-Problem 10
(*) Run-length encoding of a list.Solutions
 
Use the result of Problem 9 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:

* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
Example in Haskell:

λ> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]-}

