from queue import LifoQueue as Pila

def contar_lineas(nombre_archivos:str)->int:
    arch=open(nombre_archivos,'r')
    l=arch.readlines()
    arch.close()
    return len(l)
    
    # with open(nombre_archivos) as arch:
    
# print(contar_lineas("guia8.txt"))

def existe_palabra(palabra:str,nombre_archivo:str) ->bool:
    with open(nombre_archivo) as arch:
        lis=arch.readlines()
    if pertenece(palabra,lis):
        return True
    return False

def pertenece(s:str,l:list[str])->bool:
    for e in l:
        if s==e:
            return True
    return False

def cantidad_aparciciones(nombre_archivo:str,palabra:str)->int:
    contador=0
    with open(nombre_archivo) as arch:
        a=arch.readlines()
    for e in a:
        if palabra==e:
            contador+=1

    return contador

# print(cantidad_aparciciones("guia8.txt","hola\n"))

def clonar_sin_comentarios(nombre_archivo:str)->None:
    with open(nombre_archivo) as arch:
        a=arch.readlines()
    l=[]
    for palabra in a:
        if palabra[0] != "#":
            l.append(palabra)
    
    with open(nombre_archivo,'w') as arch:
        arch.writelines(l)

def invertir_lineas(nombre_archivo:str) -> None:
    p:Pila=Pila()
    l:list=[]
    with open(nombre_archivo) as arch:
        a=arch.readlines()
    
    for i in a:
        p.put(i)

    while not p.empty():
        l.append(p.get())

    with open(nombre_archivo,'w') as arch:
        arch.writelines(l)
    

# invertir_lineas("guia8.txt")

# clonar_sin_comentarios("guia8.txt")       

# print(existe_palabra("hola\n","guia8.txt"))

# agrupar_por_longitud("guia8.txt")

def agregar_frase_al_final(nombre_archivo:str,frase:str)->None:
    l=[]
    with open(nombre_archivo,"r") as arch:
        ar=arch.readlines()
    
    for f in ar:
        l.append(f)

    l.append("\n"+frase)
    with open(nombre_archivo,"w") as arch:
        arch.writelines(l)

def agregar_frase_al_principio(nombre_archivo:str,frase:str)->None:
    l=[]
    with open(nombre_archivo,"r") as arch:
        ar=arch.readlines()

    l.append(frase+"\n")    
    for f in ar:
        l.append(f)

    with open(nombre_archivo,"w") as arch:
        arch.writelines(l)

# agregar_frase_al_principio("guia8.txt","chau")

# def calcular_promedio_por_estudiante(nombre_archivo:str)->list:
#     with open(nombre_archivo,"r") as ar:
#         a=ar.readlines()


def separador(lista:list,separa:str)->list:
    lista.append("")
    res=""
    l=[]
    for e in lista:
        if e!=separa:
            res+=e
        else:
            l.append(res)
            res=""
    return l


# calcular_promedio_por_estudiante("guiaocho.csv")
def esplit(s:str,separa:str)->None:
    s+=" "
    ele=""
    l=[]
    for c in s:
        if c != separa:
            ele+=c
        else:
            l.append(ele)
            ele=""
    
    return l

## DICCIONARIOS
#16
def agrupar_por_longitud(nombre_archivo:str)-> dict:
    d:dict = {}
    with open(nombre_archivo) as ar:
        data = ar.read() + " "
    cc = 0
    for car in data:
        if car == " " or car == "\n":
            if not pertenece(str(cc), list(d.keys())):
                d[str(cc)] = 0
            d[str(cc)] += 1
            cc = 0
        else:
            cc += 1

def pertenec(elem,lista):
    for e in lista:
        if e == elem:
            return True
    return False

#17
def calcular_promedio_por_estudiante(notas:list[tuple[str,float]])->dict[str,float]:
    diccionario={}
    promedios={}

    for est,nota in notas:
        if not pertenec(est,promedios):
            promedios[est]=0
            diccionario[est] =0
        promedios[est] +=nota
        diccionario[est] += 1
    
    for est in promedios:
        promedios[est] /= diccionario[est]
    
    return promedios

#18
def la_palabra_mas_frecuente(nombre_archivo:str)->str:
    with open(nombre_archivo) as arch:
        a=arch.readlines()

    contador ={}
    for elemento in a:
        if not pertenec(elemento,contador):
            contador[elemento] =0
        else:    
            contador[elemento] +=1

    maximo:int=0
    res:str=""
    for string,cantidad in contador.items():
        if cantidad > maximo:
            maximo=cantidad
            res=string
    
    return res 

# print(la_palabra_mas_frecuente("guia8.txt"))
#19
def visitar_sitio(historiales:dict[str,Pila[str]],usuario:str,sitio:str)->None:

    if not pertenec(usuario,historiales):
        historiales[usuario]=Pila()
    else:
        historiales[usuario].put(sitio)
    

def navegar_atras(historiales:dict[str,Pila[str]],usuario:str):
    if pertenec(usuario,historiales):
        ultima=historiales[usuario].get()

#20
from typing import Union

def agregar_producto(inventario: dict[str, dict[str, Union[float, int]]], nombre: str, precio: float, cantidad: int):
    if not pertenec(nombre,inventario):
        inventario[nombre]={"precio":precio,"q":cantidad}

def actualizar_stock(inventario: dict[str, dict[str, Union[float, int]]], nombre: str, cantidad: int):
    if pertenec(nombre,inventario):
        inventario[nombre]["q"]+=cantidad
    
def actualziar_precios(inventario: dict[str, dict[str, Union[float, int]]], nombre: str, precio: int):
    if pertenec(nombre,inventario):
        inventario[nombre]["precio"]=precio

def calcular_valor_inventario(inventario: dict[str, dict[str, Union[float, int]]])->float:
    total=0
    for e in inventario.values():
        total+=e["precio"] *e["q"]
    return total


inventario = {}
agregar_producto(inventario, "Camisa", 20.0, 50)
agregar_producto(inventario, "Pantal´on", 30.0, 30)
actualizar_stock(inventario, "Camisa", 10)
valor_total = calcular_valor_inventario(inventario)
print("Valor total del inventario:", valor_total) # Deber´ıa imprimir 2100.0