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

def reversecasero(l:list)->list:
    lista=[]
    for e in range(len(l)-1,-1,-1):
        lista.append(l[e])
    return lista    


def cantidad_elementos(p:Pila)->int:
    contador:int=0
    l=[]
    p_aux=Pila()
    while not p.empty():
        a=p.get()
        p_aux.put(a)
        l.append(a)
        contador+=1

    r=reversecasero(l)
    for e in r:
        p.put(e)

    return contador


def buscar_el_maximo(p:Pila[int])->int:
    p_aux:Pila=Pila()
    lista:list[int]=[]
    maximo=None
    while not p.empty():
        lista.append(p.get())

    for e in lista:
        if e > maximo:
            maximo=e
    
    while not p_aux.empty():
        p.get(reversecasero(lista).pop(len(lista)-1))

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

#9
def cantidad_elementos(c:Cola)->int:
    caux:Cola=Cola()
    contador:int=0
    while not c.empty():
        caux.put(c.get())
        contador+=1
    
    while not caux.empty():
        c.put(caux.get())

    return contador




# #10
def buscar_el_maximo(c:Cola[int])->int:
    c_aux:Cola=Cola()
    l:list[int]=[]
    while not c.empty():
        a=c.get()
        c_aux.put(a)
        l.append(a)
    
    maximo=l[0]
    for e in l:
        if e>maximo:
            maximo=e
    
    while not c_aux.empty():
        c.put(c_aux.get())

    return maximo

#11

def buscar_nota_minima(c:Cola[str,int])->int:
    caux:Cola=Cola()
    l:list=[]

    while not c.empty():
        a=c.get()
        l.append(a)
        caux.put(a)
    
    m=l[0]
    for e in l:
        if e[1] < m[1]:
            m=e
    
    while not caux.empty():
        c.put(caux.get())
    return m

#12
def intercalar(c1:Cola,c2:Cola)->Cola:
    c1aux:Cola=Cola()
    c2aux:Cola=Cola()
    res:Cola=Cola()
    
    while not c1.empty() and not c2.empty():
        a=c1.get()
        c1aux.put(a)
        res.put(a)
        b=c2.get()
        c2aux.put(b)
        res.put(b)
    
    while not c1aux.empty():
        c1.put(c1aux.get())
    while not c2aux.empty():
        c2.put(c2aux.get())
    
    return res 

#13
#1
def armar_carton_de_bingo()->Cola[int]:
    res=generar_nros_al_azar(12,0,99)
    return res

#2
def pertint(a:list[int],n:int)->bool:
    for e in a:
        if e==n:
            return True
        
    return False


def jugar_carton_de_bingo(carton:list[int],bolillero:Cola[int])->int:
    b_aux:Cola[int]=Cola()
    contador:int=0
    while not bolillero.empty():
        a=bolillero.get()
        b_aux.put(a)
        contador+=1
        if pertint(carton,a):
            carton.remove(a)
        
    while not b_aux.empty():
        bolillero.put(b_aux.get())
    
    return contador






c1=Cola()
c1.put(1)
c1.put(2)
c2=Cola()
c2.put(4)
c2.put(3)
tr=intercalar(c1,c2)
