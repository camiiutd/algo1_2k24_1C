from queue import Queue as Cola
import unittest

def subsecuencia_mas_larga(v:list[int])->tuple[int,int]:
    contador=1
    indice_actual=0
    maximo_maximo=-1
    indice_maximo=-1
    for i in range(len(v)-1):
        if (v[i+1] - v[i]) == 1:
            contador+=1
        else:
            if contador > maximo_maximo:
                maximo_maximo=contador
                indice_maximo=indice_actual
            indice_actual=i+1
            contador=1
    
    if contador > maximo_maximo:
        maximo_maximo=contador
        indice_maximo=indice_actual
    
    res=(maximo_maximo,indice_maximo)
    return res

#2
def contador(e,l):
    res=0
    for num in l:
        if e == num:
            res+=1
    return res


def mejor_resultado_de_ana(examenes:Cola[list[bool]])->list[int]:
    restauro_cola=[]
    res=[]
    while not examenes.empty():
        e=examenes.get()
        restauro_cola.append(e)
        
        q_true=contador(True,e)
        q_false=contador(False,e)

        if q_false == 0 and q_true > 0:
            a=q_true // 2
            res.append(a)
        elif q_true == 0 and q_false > 0:
            b=q_false // 2
            res.append(b)
        elif q_true > q_false and q_false != 0:
            res.append(q_true)
        elif q_false > q_true and q_true != 0:
            res.append(q_false)
        elif q_false == q_true:
            res.append(q_true + q_false)
    
    for elemento in restauro_cola:
        examenes.put(elemento)

    return res

def crear_cola(elementos):
    cola = Cola()
    for elem in elementos:
        cola.put(elem)
    return cola

def copiar_cola(cola: Cola):
    elementos = list(cola.queue)
    nueva_cola = Cola()
    for e in elementos:
        nueva_cola.put(e)
    return nueva_cola


#3
# def mtraspuesta(matriz:list[list[int]])->list[list[int]]:
#     res=[]
#     for i in range(len(matriz[0])):
#         columa=[]
#         for j in range(len(matriz)):
#             columa.append(matriz[j][i])
#         res.append(columa)
#     return res

def reverso_matriz(matriz:list[list[int]])->list[list[int]]:
    res=[]
    for m in range(len(matriz)-1,-1,-1):
        res.append(matriz[m])
    return res

def cambiar_matriz(A:list[list[int]]):

    las_doyVuelta=reverso_matriz(A)

#4
def contador_vocales(texto):
    res=0
    v='AEIOUaeiou'
    for c in texto:
        if pertenece(c,v):
            res+=1
    return res

def pertenece(e,l):
    for i in l:
        if e == i :
            return True
        
    return False

def esplit(texto,separador):
    guardo_palabra=""
    res=[]
    texto+=separador
    for c in texto:
        if c == separador:
            if not guardo_palabra == '':
                res.append(guardo_palabra)
                guardo_palabra=""
        else:
            guardo_palabra+=c

    return res

def palabras_por_vocales(texto:str)->dict[int,int]:
    res:dict={}
    saco_espacios=esplit(texto," ")
    print(saco_espacios)
    for palabra in saco_espacios:
        q_vocales=contador_vocales(palabra)
        if not pertenece(q_vocales,res):
            res[q_vocales] = 0
        res[q_vocales] +=1
    
    return res

print(palabras_por_vocales(" a e  i F X i M W u"))


        
                

