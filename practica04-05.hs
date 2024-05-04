{-Ejercicio 5. Definir las siguientes funciones sobre listas:
1. sumaAcumulada :: (Num t) => [t] -> [t] seg´un la siguiente especificaci´on:
problema sumaAcumulada (s: seq⟨T⟩) : seq⟨T⟩ {
requiere: {T es un tipo num´erico}
requiere: {cada elemento de s es mayor estricto que cero}
asegura: {|s| = |resultado| ∧ el valor en la posici´on i de resultado es Pi
k=0 s[k]}
}
Por ejemplo sumaAcumulada [1, 2, 3, 4, 5] es [1, 3, 6, 10, 15].
-}

suma :: (Num t) => [t] -> t
suma [] = 0
suma (x:xs) = head (x:xs) + sumaAux xs

sumaAux :: (Num t) => [t] -> t
sumaAux (x:xs) = suma (x:xs)


sumaAcumulada :: (Eq t , Num t) => [t] -> [t]
sumaAcumulada [x] = [x]
sumaAcumulada (x:xs) | suma (x:xs) == x = head (x:xs) : sumaAux xs : []
                     | otherwise = sumaAcumulada xs