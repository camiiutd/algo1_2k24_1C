from queue import Queue as Cola
def orden_de_atencion(urgentes:Cola[int],postergables:Cola[int])->Cola[int]:
    res:Cola=Cola()
    urg:Cola=Cola()
    post:Cola=Cola()
    while not urgentes.empty() and not postergables.empty():
        a=urgentes.get()
        res.put(a)
        urg.put(a)
        b=postergables.get()
        res.put(b)
        post.put(b)
    
    while not urg.empty():
        urgentes.put(urg.get())
    
    while not post.empty():
        postergables.put(post.get())   
    
    return res


# c=Cola()
# c.put(1)
# c.put(3)
# c.put(4)
# d=Cola()
# d.put(7)
# d.put(6)
# d.put(55)

# resultado= orden_de_atencion(c,d)

# while not resultado.empty():
#     print(resultado.get())

#2
def pertenece(lista,e):
    for i in lista:
        if i == e:
            return True
    return False

# def alarma_epidemiologica(registros:list[tuple[int,str]],infecciosas:list[str],umbral:float)->dict[str,float]:
#     condicion:float= len(registros)
#     dic:dict={}
#     for e in registros:
#         i=e[1]
#         contador:int=0
#         if not pertenece(infecciosas,i):
#             dic[i]=0
#             contador+=1
#         else:
#             dic[i]+=contador/condicion
        
#     for llave,contra in dic.items():
#         if umbral < contra:
#             dic.pop(llave)

#     return dic

# registros = [(1, "gripe"), (2, "gripe"), (3, "resfriado"), (4, "gripe")]
# infecciosas = ["gripe", "resfriado"]
# umbral = 0.5

# resultado = alarma_epidemiologica(registros, infecciosas, umbral)
# print(resultado)  # Salida esperada: {'gripe': 0.75}

#3
def sumatoria(l:list[int])->int:
    res:int=0
    for i in l:
        res+=i
    return res

print(sumatoria([4,5,5]))

def maxim(l:list[int])->int:
    maximo=l[0]
    for i in l:
        if i >= maximo:
            maximo=i
    return maximo


def empleados_del_mes(horas:dict[int,list[int]])->list[int]:
    res:list[int]=[]
    maximo=0
    for k,l in horas.items():
        suma:int=sumatoria(l)
        if suma > maximo:
            maximo=suma

    for k,l in horas.items():
        if sumatoria(l) ==maximo:
            res.append(k)       

    return res

#4
def nivel_de_ocupacion(camas_por_piso:list[list[bool]])->list[float]:
    res:list[float]=[]
    for piso in camas_por_piso:
        cantidad:int=len(piso)
        contador:int=0
        for cama in piso:
            if cama==True:
                contador+=1
        res.append(contador/cantidad)
    
    return res

print(nivel_de_ocupacion([[True,False,True],[True,True,True]]))

        



