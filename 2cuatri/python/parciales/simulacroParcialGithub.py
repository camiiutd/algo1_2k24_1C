# https://github.com/laurabailleres/introprogramacion/blob/main/simulacroParcialPython.py

#1
def ultima_aparicion(s: list, e: int) -> int:
    res:int=0
    for i in range(len(s)):
        if e==s[i]:
            res=i
    return res

# s = [-1,4,0,4,100,0,100,0,-1,-1]
# e = -1
# print(ultima_aparicion(s,e))

#2
def pertenece(elemento,lista):
    for e in lista:
        if e ==elemento:
            return True
    return False

def elementos_exclusivos(s: list, t: list) -> list:
    res:list=[]
    for e in s:
        if not pertenece(e,t):
            if not pertenece(e,res):
                res.append(e)
    
    for j in t:
        if not pertenece(j,s):
            if not pertenece(j,res):
                res.append(j)
        
    return res

# s = [-1,4,0,4,3,0,100,0,-1,-1]
# t = [0,100,5,0,100,-1,5]
# print(elementos_exclusivos(s,t))

#3
def contar_traducciones_iguales(ingles: dict, aleman: dict) -> int:
    contador:int=0
    for keys,values in ingles.items():
        for llaves,valores in aleman.items():
            if keys == llaves and values==valores:
                contador+=1
    return contador

# aleman = {"Mano": "Hand", "Pie": "Fuss", "Dedo": "Finger", "Cara": "Gesicht"}
# inglés = {"Pie": "Foot", "Dedo": "Finger", "Mano": "Hand"}
# print(contar_traducciones_iguales(inglés,aleman))

#4
def convertir_a_diccionario(lista: list) -> dict:
    res:dict={}
    for e in lista:
        if not pertenece(e,res):
            res[e]=0
    
    for e in lista:
        res[e] +=1
    
    return res

lista = [-1,0,4,100,100,-1,-1]
print(convertir_a_diccionario(lista))