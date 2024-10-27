#1
#1.1
def pertenece(s:list[int],e:int)->bool:
    for i in s:
        if e==i:
            return True
    return False
    
#1.2
def divide_a_todos(s:list[int],e:int)->bool:
    for elemento in s:
        if elemento%e==0:
            return True
        else:
            return False

#1.3       
def suma_total(s:list[int])->int:
    res:int=0
    for elemento in s:
        res+=elemento
    return res

#1.4
def maximo(s:list[int])->int:
    maximo:int=0
    for elemento in s:
        if elemento>=maximo:
            maximo=elemento
        else:
            continue
    return maximo

#1.5
def minimo(s:list[int])-> int:
    minimo:int=0
    for ele in s:
        if ele <=minimo:
            minimo=ele
        else:
            continue
    return minimo

#1.6
def ordenados(s:list[int])->bool:
    for e in range(len(s)-1):
        if s[e] >= s[e+1]:
            return False
    return True

#1.7
def pos_maximo(s:list[int])->int:
    contador:int=0
    if len(s)==0:
        return -1
    
    for e in range(0,len(s)):
        if s[e] >= s[contador]:
            contador=e

    return contador

#1.8
def pos_minimo(s:list[int])->int:
    contador:int=0
    if len(s)==0:
        return -1

    for e in range(0,len(s)):
        if s[e]<= s[contador]:
            contador=e
    return contador

#1.9
def palabra7(s:list[str])->bool:
    for i in s:
        if len(i) > 7:
            return True
    return False

#1.10
def es_capicua(t:str)-> bool:
    lista=armolista(t)
    lista_capicua=reverso(armolista(t))
    for c in range(0,len(lista)):
        if lista[c] != lista_capicua[c]:
            return False
    return True

def armolista(t:str)->list[str]:
    lista=[]
    for letra in t:
        lista.append(letra)
    return lista

def reverso(l:list[str])->list[str]:
    res=l[::-1]
    return res

def palindromo(t:str)->bool:
    return t==t[::-1]

#1.11
def numsiguales(l:list[int])-> bool:
    for e in range(len(l)-1):
        if l[e] == l[e+1]:
            return True
    return False

# 1.12
def numiguales3(l:list[int])->bool:
    for e in range(len(l)-1):
        if l[e] == l[e+1] and l[e+1] == l[e+2]:
            return True
    return False

# 1.13

def secuenciaslargas(l:list[int])-> int:
    lista:list[int]=[]
    res:int=0
    contador:int=0
    for e in range(len(l)-1):
        if l[e] <= l[e+1]:
            lista.append(e)
            contador+=1
        else:
            if len(l) >= contador:
                lista.clear()
                contador=0
            else:
                continue
    return e


#1.14
def cantidad_digitos_impares(s:list[int])->int:
    contador:int=0
    lista:list=[]
    for char in s:
        for dig in str(char):
            if int(dig)%2==1:
                contador+=1    
    return contador

# print(cantidad_digitos_impares([57, 2383, 812, 246]))

#2
#2.1
def cerosenposicionespares(s:list[int])->list[int]:
    lista:list[int]=[]
    for e in range(len(s)):
        if e%2==0:
            lista.append(0)
        else:
            lista.append(s[e])
    return lista

#2.3
def sinvocales(s:str)-> str:
    vocales:str="AIUEOaiueo"
    res:str=""
    for char in s:
        if char not in vocales:
            res+=char
    return res

#2.4
def reemplaza_vocales(s:list[str])->list[str]:
    vocales:str="AIUEOaiueo"
    res:str=""
    for char in s:
        if char in vocales:
            res+="-"
        else:
            res+=char
    return res

#2.5
def da_vuelta_str(s:str)-> str:
    res=s[::-1]
    return res

#2.6
def eliminar_repetidos(s:str)-> str:
    res:str=""
    for char in s:
        if char not in res:
            res+=char
    return res

#2 ej 3
def resultadoMateria(notas:list[int])->int:
    for n in notas:
        if n < 4 or promedio(notas) < 4:
            return 3
        elif n >=4 and( promedio(notas) >=4 and promedio(notas) <7):
            return 2
        else: 
            return 1

def promedio(l:list[int])->float:
    sumatotal:int=0
    res:float=0
    for e in l:
        sumatotal+=e
    res=sumatotal/len(l)
    return res

#2 ej 4
def datos_bancarios(t:list[tuple[str,int]])-> int:
    saldo:int=0
    for e in t:
        if e[0] == "I":
            saldo+=e[1]
        elif e[0] == "R":
            saldo-=e[1]
    return saldo

#matrices
#3.1
def pertenece_a_cada_uno_v1(s:list[list[int]],e:int)->list[bool]:
    res:list[bool]=[]
    for matriz in s:
        if e in matriz:
            res.append(True)
        else:
            res.append(False)
    return res

#3.2
# si ahi pienso

#3.6
#1 
def es_matriz(s:list[list[int]]): #->bool:
    longitud:int=len(s[0])
    
    for m in s:
        if len(m) != longitud:
            return False
    return True

#2 
def orden(s:list[int])->bool:
    for e in range(len(s)-1):
        if s[e] >= s[e+1]:
            return False
    return True
    
def filas_ordenadas(m:list[list[int]])->list[bool]:
    res:list[bool]=[]
    for i in m:
        if orden(i):
            res.append(True)
        else:
            res.append(False)
    return res

def columna(m:list[list[int]],c:int)->list[int]:
    columna:list[int]=[]
    for i in range(len(m)):
        columna.append(m[i][c])
    return columna

def columna_ordenadas(m:list[list[int]])->list[bool]:
    res:list[bool]=[]
    n=len(m[0]) 
    for c in range(n):
        col=columna(m,c) 
        if orden(col):
            res.append(True)
        else:
            res.append(False)
    return res


def transponer(m:list[list[int]])-> list[list[int]]:
    res:list[list[int]]=[]
    for c in range(len(m[0])):
        res.append(columna(m,c))
    return res


# # 6 tateti
def tatetiHoV(m:list[str])->str:
    contador:int=0
    if pertenece(m,"X"):
        contador+=1

    if contador==3:
        return "X"
    if contador==0:
        return "O"
    else:
        return "Y"   #Ninguno de los dos

def quien_gana_tateti(m:list[list[str]])-> int:
    for i in m:
        if tatetiHoV(i) =="X":
            return 1
        elif tatetiHoV(i) =="O":
            return 0
    
    for e in transponer(m):
        if tatetiHoV(e)=="X":
            return 1
        elif tatetiHoV(e)=="O":
            return 0
        
    if m[0][0] == m[1][1] == m[2][2]:
        if m[0][0] == "X":
            return 1
        else:
            return 0
        
    if m[0][2] == m[1][1] == m[2][0]:
        if m[0][2] == "X":
            return 1
        else:
            return 0
    
    return 2
    

        
# print(quien_gana_tateti([
#     ['O', 'O', 'X'],
#     ['X', 'X', 'O'],
#     ['O', 'O', 'X']
# ]))

#me salteo el opcional

#4
#7.1
def tironombres()->list[str]:
    res:list[str]=[]
    while True:
        nombre=input("Ingrese nombres pofi --> ")
        if nombre=="listo" or nombre==" ":
            break
        else:
            res.append(nombre)
    return(res)
    
#7.2
def sube():
    res=[]
    while True:
        accion=input("Ingrese acción (C,D,X): ")
        if accion=="C" or accion=="D":
            monto:int=int(input("Ingrese monto --> "))
            res.append((accion,monto))
        elif accion == "X":
            break
        else:
            print("Ingrese una acción permitida")
    return res

#7.3
import numpy as np
import random

def sieteymedio():
    hist:list[int]=[]
    contador:int=0
    while contador <= 7.5:
        accion=input("Saca carta? (SI o NO): ")
        if accion=="SI":
            numero=random.randint(1,12)
            if numero==10 or numero==11 or numero==12:
                contador+=0.5
                hist.append(0.5)
            elif numero==8 or numero==9:
                continue
            else:
                contador+=numero
                hist.append(numero)
        elif accion=="NO":
            print("Zafaste, tenes ",contador)
            break
        else:
            print("INGRESE SOLO SI O NO!!!!!!!!!")
    if contador > 7.5:
        print("Perdiste tenes",contador)

    return hist 

#7.4
def pertenec(l:list,n):
    for e in l:
        if e ==n:
            return True
    return False

def contrasenia()->str:
    mayusculas="QWERTYUIOPASDFGHJKLZXCVBNM"
    minusculas="qwertyuiopasdfghjklzxcvbnm"
    numeros="0123456789"
    mayu:bool=False
    minu:bool=False
    num:bool=False
    contra=input("Ingrese una contraseña: ")
    
    for c in contra:
        if pertenec(mayusculas, c):
            mayu = True
        elif pertenec(minusculas, c):
            minu = True
        elif pertenec(numeros, c):
            num = True

    if mayu and num and minu and len(contra) >=8:
        return "VERDE"
    elif len(contra) < 5:
        return "ROJO"
    else:
        return "AMARILLO"


print(contrasenia())