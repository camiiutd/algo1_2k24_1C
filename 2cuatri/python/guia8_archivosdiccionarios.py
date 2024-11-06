from queue import LifoQueue as Pila

def contar_lineas(nombre_archivos:str)->int:
    arch=open(nombre_archivos,'r')
    l=arch.readlines()
    arch.close()
    return len(l)
    
    # with open(nombre_archivos) as arch:
    
print(contar_lineas("guia8.txt"))

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

## diccionarios

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
    print(d)

agrupar_por_longitud("guia8.txt")
