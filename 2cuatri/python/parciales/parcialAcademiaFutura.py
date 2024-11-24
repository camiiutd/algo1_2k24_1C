from queue import Queue as Cola

def pertenece(item,l)->bool:
    for e in l:
        if e==item:
            return True
    return False

def gestion_notas(notas_estudiante_materia:list[tuple[str,str,int]])->dict[str,list[str,int]]:
    res:dict={}
    for elemento in notas_estudiante_materia:
        nombre=elemento[0]
        materia=elemento[1]
        nota=elemento[2]
        if not pertenece(elemento,res):
            res[nombre]=(materia,nota)
    
    return res

#2
def cantidad_digitos_pares(numeros:list[int])->int:
    contador:int=0
    for elemento in numeros:
        num=str(elemento)
        for c in num:
            if int(c)%2==0:
                contador+=1
    
    return contador

#3

def reversecasero(l:list)->list:
    lista=[]
    for e in range(len(l)-1,-1,-1):
        lista.append(l[e])
    return lista    

def cola_primero_pesados(paquetes:Cola[tuple[str,int]],umbral:int)->Cola[tuple[str,int]]:
    res:Cola=Cola()
    cola_guardado:Cola=Cola()
    lista_ligeros:list[tuple[str,int]]=[]
    while not paquetes.empty():
        elemento:tuple[str,int]=paquetes.get()
        cola_guardado.put(elemento)

        if elemento[1] > umbral:
            res.put(elemento)
        else:
            lista_ligeros.append(elemento)
    
    for tupla in lista_ligeros:
        res.put(tupla)
    
    while not cola_guardado.empty():
        paquetes.put(cola_guardado.get())

    return res
        

def descolar(d:Cola):
    l=[]
    while not d.empty():
        l.append(d.get())

    return l

#4
def matriz_traspuesta(m:list[list[int]])->list[list[int]]:
    res:list[list[int]]=[]
    for indice in range(len(m[0])):
        columna=[]
        for elemento in range(len(m)):
            columna.append(m[elemento][indice])
        res.append(columna)
    return res

def minimo(lista:list[int])->int:
    minimo=lista[0]
    for e in lista:
        if e < minimo:
            minimo=e
    
    return minimo

def matriz_pseudo_ordenada(matriz:list[list[int]])->bool:
    matriznueva=matriz_traspuesta(matriz)
    for elemento in range(len(matriznueva)-1):
        mini=minimo(matriznueva[elemento])
        mini_siguiente=minimo(matriznueva[elemento+1])
        if mini >= mini_siguiente:
            return False
    return True

