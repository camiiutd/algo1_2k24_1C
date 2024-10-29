#pilas
#1
from queue import LifoQueue as Pila
import numpy as np


def generar_nros_al_azar(cantidad:int,desde:int,hasta:int)->Pila[int]:
    p=Pila()
    while cantidad > 0:
        p.put(np.random.randint(desde,hasta))
        cantidad-=1
    return p

def cantidad_elementos(p:Pila)->int:
    contador:int=0
    p_aux=Pila()
    while not p.empty():
        p_aux.put(p.get())
        contador+=1
    while not p_aux.empty():
        p.put(p_aux.get())
    return contador

def buscar_el_maximo(p:Pila[int])->int:
    p_aux:Pila=Pila()
    lista:list[int]=[]
    maximo=None
    while not p.empty():
        lista.append(p.get())

    for e in lista:
        if e >= maximo:
            maximo=e
    
    while not p_aux.empty():
        p.get(lista.pop(len(lista)-1))

    return maximo

def buscar_nota_maxima(p:Pila[tuple[str,int]])-> tuple[str,int]:
    p_aux:Pila=Pila()
    maximo=None
    while not p.empty():
        m= p.get()
        p_aux.put(m)
    
        if maximo==None:
            maximo=m
        else:
            if m[1] > maximo[1]:
                m=maximo
            
    while not p_aux.empty():
        p.put(p_aux.get())

    return maximo

def esta_bien_balanceada(s:str) -> bool:
    contador:int=0
    for c in s:
        if c =="(":
            contador+=1
        elif c==")":
            contador-=1
    
    if contador==0:
        return True
    return False

def evaluar_expresion(s:str)->float:
    res=Pila()
    num="1234567890"
    op="+-*/"
    
    l=splits(s," ")
    for e in l:
        if pertenece(num,e):
            res.put(float(e))
        elif pertenece(op,e):
            a=res.get()
            b=res.get()            
            if e=="+":
                res.put(b+a)
            elif e=="-":
                res.put(b-a)
            elif e=="*":
                res.put(b*a)
            elif e=="/":
                res.put(b/a)
    return res.get()
    


def splits(string,limitador):
    l=[]
    res=""
    string+=limitador
    for c in string:
        if c!= limitador:
            res+=c
        else:
            l.append(res)
            res=""
    return l
    
def pertenece(s:str,m:str)-> bool:
    for c in s:
        if c==m:
            return True
    return False

def intercalar(p1:Pila,p2:Pila)->Pila:
    pila_aux:Pila=Pila()
    p1recupero:Pila=Pila()
    p2recupero:Pila=Pila()
    while not p1.empty() and not p2.empty() :
        a=p1.get()
        pila_aux.put(a)
        p1recupero.put(a)
        b=p2.get()
        pila_aux.put(b)
        p2recupero.put(b)

    while not p1recupero.empty():
        p1.put(p1recupero.get())

    while not p2recupero.empty():
        p2.put(p2recupero.get())

    return pila_aux


# p1 = Pila()
# p1.put(1)
# p1.put(2)
# p1.put(3)

# p2 =Pila()
# p2.put(3)
# p2.put(2)
# p2.put(1)

# print(intercalar(p1,p2))

# a=Pila()
# a.put(("a",5))
# a.put(("e",4))
# a.put(("p",8))
# print(buscar_nota_maxima(a))

# print(splits("hola aa a a"," "))

# print(evaluar_expresion("3 4 + 5 * 2 -"))

#COLAS
#8
from queue import Queue as Cola

def generar_nros_al_azar(cantidad:int,desde:int,hasta:int)->Cola[int]:
    c=Cola()
    while cantidad >0:
        c.put(np.random.randint(desde,hasta))
        cantidad-=1
    return c

c=generar_nros_al_azar(5,1,10)
