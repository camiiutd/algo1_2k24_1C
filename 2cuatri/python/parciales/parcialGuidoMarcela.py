def ind_nesima_aparicion(s: list, n: int, elem: int) -> int:
    contador:int=0
    res:int=-1
    
    for i in range(len(s)):
        if s[i] == elem:
            contador+=1
            if contador == n:
                res=i

    return res


# s = [-1, 1, 1, 5, -7, 1, 3]
# n = 2
# elem = 1
# print(ind_nesima_aparicion(s,n,elem))

#2
def mezclar(s1: list, s2: list) -> list:
    res=[]
    for i in range(len(s1)):
        for j in range(len(s2)):
            if i==j:
                res.append(s1[i])
                res.append(s2[j])

    return res

# s1 = [1, 3, 0, 1]
# s2 = [4, 0, 2, 3]
# # se debería devolver res = [1, 4, 3, 0, 0, 2, 1, 3]
# print(mezclar(s1,s2))

#3
