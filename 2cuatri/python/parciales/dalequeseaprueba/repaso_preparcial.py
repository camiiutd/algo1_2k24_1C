def subsecuencia_mas_larga(v:list[int])->tuple[int,int]:
    cont_maxima=-1
    contador=1
    i_actual=0
    i_maximo=-1
    for num in range(len(v)-1):
        if (v[num+1] - v[num]) == 1 or (v[num+1] - v[num]) == -1:
            contador+=1
        else:
            if contador > cont_maxima:
                cont_maxima=contador
                i_maximo=i_actual
            i_actual=num+1
            contador=1
    
    if contador > cont_maxima:
        cont_maxima=contador
        i_maximo=i_actual

    res=(cont_maxima,i_maximo)
    return res

def m_traspuesta(matriz:list[list[int]])->list[list[int]]:
    res=[]
    for i in range(len(matriz[0])):
        col=[]
        for j in range(len(matriz)):
            col.append(matriz[j][i])
        res.append(col)
    return res

def maximo(l):
    max_actual=l[0]
    for e in l:
        if e > max_actual:
            max_actual=e
    return max_actual

def minimo(l):
    mini=l[0]
    for i in l:
        if i < mini:
            mini=i 
    return mini    
        
def creo_matriz(l:list[int])->list[list[int]]:
    matriz=[]
    maxi=maximo(l)
    mini=minimo(l)
    
    n= maxi - mini +1

    for i in range(n):
        fila=[]
        for j in range(n):
            valor= mini + (i+j) % n
            fila.append(valor)
        matriz.append(fila)

    return matriz

#4
def resolver_cuenta(s:str)->float:
    operacion:int=1
    numero_acumulado=0
    parte_decimal=0
    enterolisto=False
    res=0
    for n in s:
        if n=='+' or n=='-':
            res += float(str(numero_acumulado)+'.'+str(parte_decimal)) * operacion
            numero_acumulado=0
            parte_decimal=0
            enterolisto=False
    
            if n == "+":
                operacion=1
            elif n == "-":
                operacion=-1
        elif n == ".":
            enterolisto = True
        elif enterolisto==True:
            parte_decimal = parte_decimal * 10 + int(n)
        else:
            numero_acumulado = numero_acumulado * 10 + int(n)

    res+=float(str(numero_acumulado)+'.'+str(parte_decimal))  * operacion

    return res



        
