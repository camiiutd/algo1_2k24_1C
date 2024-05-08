{-Problem 15
(**) Replicate the elements of a list a given number of times.Solutions
Example:

* (repli '(a b c) 3)
(A A A B B B C C C)
Example in Haskell:

λ> repli "abc" 3
"aaabbbccc"-}

repli :: [t] -> Int -> [t]
repli [] _ = []
repli (x:xs) n = repliaux x n ++ repli xs n

repliaux :: t -> Int -> [t]
repliaux _ 0 = []
repliaux x n = x : repliaux x (n-1)

--
{-Problem 16
(**) Drop every N'th element from a list.Solutions
 
Example:

* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)
Example in Haskell:

λ> dropEvery "abcdefghik" 3
"abdeghk"-}

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = [] 
dropEvery n xs = dropEvery' n xs 1 

dropEvery' :: Int -> [a] -> Int -> [a]
dropEvery' _ [] _ = [] 
dropEvery' n (x:xs) count
    | count == n = dropEvery' n xs 1 
    | otherwise = x : dropEvery' n xs (count + 1)

-- Ejemplo de uso:
-- dropEvery 3 "abcdefghik" devuelve "abdeghk"
