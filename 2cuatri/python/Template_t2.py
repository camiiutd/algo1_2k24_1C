from queue import Queue as Cola

# Ejercicio 1
def pertenece(l,s):
    for i in l:
        if i==s:
            return True
    return False


def gestion_ventas(ventas_empleado_producto: list[tuple[str, str, int]]) -> dict[str, list[tuple[str,int]]]:
    res:dict={}
    for i in ventas_empleado_producto:
        nombre:str=i[0]
        item:str=i[1]
        q:int=i[2]
        if not pertenece(nombre,res):
            l=[]
            res[nombre]=[l]
        
        for k,ll in res.items():
            if k==nombre:
                l.append((item,q))
        
    return res



 
# Ejercicio 2

def paso_numero(l:list[int])->list[int]:
    res=[]
    r=""
    for e in l:
        r+=str(e)
        for i in r:
            res.append(i)
            r=""
    return res


def cantidad_digitos_impares(numeros: list[int]) -> int:
    res:int=0
    digitos:list=paso_numero(numeros)
    for n in digitos:
        if int(n)%2==1:
            res+=1
    
    return res
    


        

# Ejercicio 3
def reordenar_cola_primero_numerosas(carpetas: Cola[tuple[str,int]], umbral:int) -> Cola[tuple[str,int]]:
    res_cola:Cola=Cola()
    recupero:Cola=Cola()
    menores:Cola=Cola()
    while not carpetas.empty():
        carp=carpetas.get()
        id=carp[0]
        num=carp[1]

        recupero.put((id,num))

        if num > umbral:
            res_cola.put((id,num))
        elif num <= umbral: 
            menores.put((id,num))

    while not menores.empty():
        res_cola.put(menores.get())

    while not recupero.empty():
        carpetas.put(recupero.get())

    return res_cola




# Ejercicio 4

def columna(matriz: list[list[int]]) -> list[int]:
    res:list[int]=[]

    for i in range(len(matriz[0])):
        l=[]
        for e in range(len(matriz)):
            l.append(matriz[e][i])
        res.append(l)
         
    return res



def matriz_cuasi_decreciente(matriz: list[list[int]]) -> bool:
    matriz_t=columna(matriz) 
    maximo=0
    for e in matriz_t:
        for i in e:
            if i > maximo:
                maximo=i
            return False
            
    
        return True




    
        



            



    


# print(matriz_cuasi_decreciente([[1,2,3],[5,1,1],[2,3,2]]))    
    

