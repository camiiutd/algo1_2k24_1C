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
def pertenece(elemento,l):
    for e in l:
        if e==elemento:
            return True
    return False

def creo_lista_posiciones(lista:list,caballo:str)->list[int]:
    l=[0]*len(lista)
    for i in range(len(lista)):
        if caballo==lista[i]:
            l[i]+=1

    return l
# print(creo_lista_posiciones(["hola","ww","e"],"hola"))

def frecuencia_posiciones_por_caballo(caballos: list, carreras: dict) -> dict:
    res:dict={}
    for c in caballos:
        if not pertenece(c,res):
            res[c]=[0]*len(caballos)
    
    for c in caballos:
        for carrera in carreras.values():
            for i in range(len(carrera)):
                if carrera[i]==c:
                    res[c][i]+=1
    return res


# caballos= ["linda", "petisa", "mister", "luck" ]
# carreras= {"carrera1":["linda", "petisa", "mister", "luck"],
#                  "carrera2":["petisa", "mister", "linda", "luck"]}

# print(frecuencia_posiciones_por_caballo(caballos,carreras))

def reverso(l:list)->list:
    res=[]
    for e in range(len(l)-1,-1,-1):
        res.append(l[e])
    return res

def matriz_capicua(m: list) -> bool:
    for fila in m:
        capicua=reverso(fila)
        if fila != capicua:
            return False
    
    return True

# m = [[1,2,2,1],[-5,6,6,-5],[0,1,1,0]]
# print(matriz_capicua(m))


