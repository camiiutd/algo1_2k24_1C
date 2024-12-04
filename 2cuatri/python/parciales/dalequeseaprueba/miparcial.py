from queue import LifoQueue as Pila


def multiplos_de_primos(v:list[int])->dict[int,int]:
    res:dict={}
    l:list=[]
    for num in v:
        lista_de_divisores=te_doy_primo(num,2)
        for n in lista_de_divisores:
            l.append(n)

    for e in l:
        if not pertenece(e,res):
            contador=cont(e,l)
            res[e] = contador

    return res

def cont(elem,l):
    res:int=0
    for e in l:
        if elem==e:
            res+=1
    return res

def pertenece(elem,l):
    for e in l:
        if e == elem:
            return True
    return False 

def te_doy_primo(n:int,m:int)->list[int]:
    res:list[int]=[]
    while m <= n:
        if n%m == 0:
            if not pertenece(m,res):
                res.append(m)
            n = n//m
            
        else:
            m+=1
    
    return res

#2
def longitud_mas_grande(A:list[list[int]])->int:
    res:list=[]
    maximo_guardado:int=0
    maximo_actual:int=0
    for e in A:
        for num in e:
            res.append(num)
        res.append(0)
    
    for elemento in res:
        if elemento == 1:
            maximo_actual +=1
    
        else:
            if maximo_actual > maximo_guardado:
                maximo_guardado = maximo_actual
                maximo_actual =0
        
    if maximo_actual > maximo_guardado:
        maximo_guardado = maximo_actual
    
    return maximo_guardado

#3

def separador(string:str,sep:str)->list:
    res=[]
    string+=sep
    guardo_palabra=""
    for elemento in string:
        if elemento == sep:
            res.append(guardo_palabra)
            guardo_palabra=""
        else:
            guardo_palabra+=elemento

    return res


def resuelvo(cuenta: str) -> int:
    res = 0
    operador = '+'
    i =0
    
    while i < len(cuenta):
        if pertenece(cuenta[i], '+-'):  
            operador = cuenta[i]
            i += 1
        else:  
            numero = 0
            while i<len(cuenta) and not pertenece(cuenta[i],'+-'):  
                numero=numero * 10 + int(cuenta[i])
                i+=1

            if operador=='+':
                res += numero
            elif operador== '-':
                res -=numero

    return res

def reverso(l)->list:
    res=[]
    for e in range(len(l)-1,-1,-1):
        res.append(l[e])
    return res

def resolver_cuentas(p:Pila[str])->list[int]:
    restauro_pila=[]
    res:list[int]=[]
    while not p.empty():
        cuenta=p.get()
        restauro_pila.append(cuenta)

        respuesta=resuelvo(cuenta)
        res.append(respuesta)
    
    doy_vuelta=reverso(restauro_pila)

    for elemento in doy_vuelta:
        p.put(elemento)

    return res

def crear_pila(elementos):
    pila = Pila()
    for elem in elementos:
        pila.put(elem)
    return pila
#4
def saco_maximo(l)->int:
    maximo=l[0]
    for num in l:
        if num > maximo:
            maximo=num
    return maximo


def dame_el_que_falta(s:list[tuple[int,int]])->tuple[int,int]:
    l=[]
    for e in s:
        for num in e:
            l.append(num)
    
    maximo=saco_maximo(l)

    for i in range(1,maximo+1):
        for j in range(1,maximo+1):
            tupla=(i,j)
            if not pertenece(tupla,s):
                return tupla
    