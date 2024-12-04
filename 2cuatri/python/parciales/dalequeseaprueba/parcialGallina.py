from queue import Queue as Cola

def pertenece(elemento,l):
    for e in l:
        if e ==elemento:
            return True
    return False


def reversa(l:list)->list:
    res=[]
    for e in range(len(l)-1,-1,-1):
        res.append(l[e])
    return res


def torneo_de_gallinas(estrategias: dict[str, str]) -> dict[str, int]:
    res:dict[str,int] = {}

    for nombre in estrategias.keys():
        res[nombre]=0

    nombres_lista = list(estrategias.keys()) 
    
    for i in range(len(nombres_lista)):
        for j in range(i + 1, len(nombres_lista)):
            nombre1 = nombres_lista[i]
            nombre2 = nombres_lista[j]
            estrategia1 = estrategias[nombre1]
            estrategia2 = estrategias[nombre2]
            
            
            if estrategia1 == "me la banco y no me desvio" and estrategia2 == "me la banco y no me desvio":
                res[nombre1] -= 5
                res[nombre2] -= 5
            elif estrategia1 == "me desvio siempre" and estrategia2 == "me desvio siempre":
                res[nombre1] -= 10
                res[nombre2] -= 10
            elif estrategia1 == "me desvio siempre" and estrategia2 == "me la banco y no me desvio":
                res[nombre1] -= 15
                res[nombre2] += 10
            elif estrategia1 == "me la banco y no me desvio" and estrategia2 == "me desvio siempre":
                res[nombre1] += 10
                res[nombre2] -= 15

    return res


# estrategias = {
#     "Juan": "me la banco y no me desvio",
#     "Maria": "me desvio siempre",
#     "Pedro": "me la banco y no me desvio"
# }

# print(torneo_de_gallinas(estrategias))

#2
def reordenar_cola_priorizando_vips(filaClientes:Cola[tuple[str,str]])->Cola[str]:
    restaurocola:Cola=Cola()
    res:Cola=Cola()
    comunes:list[str]=[]
    while not filaClientes.empty():
        cliente=filaClientes.get()
        restaurocola.put(cliente)

        if cliente[1] == "vip":
            res.put(cliente)
        else:
            comunes.append(cliente)

    for persona in comunes:
        res.put(persona)
    
    while not restaurocola.empty():
        filaClientes.put(restaurocola.get())
    
    return res

#3
def reverso(l:list)->list:
    res:list=[]
    for i in range(len(l)-1,-1,-1):
        res.append(l[i])
    return res

def texto_a_letras(texto:str)->list[str]:
    res=[]
    for letra in texto:
        res.append(letra)
    return res

def cuantos_sufijos_son_palindromos(texto:str)->int:
    listaLetras=texto_a_letras(texto)
    listaLetrasReverso=reverso(listaLetras)
    contador:int=0

    while listaLetras:
        if listaLetras == listaLetrasReverso:
            contador+=1
        
        listaLetras.pop(0)
        listaLetrasReverso.pop(-1)

    return contador

# print(cuantos_sufijos_son_palindromos("aaaa"))                

#4
def matriz_traspuesta(tablero:list[list[str]])->list[list[str]]:
    res:list[str]=[]
    for i in range(len(tablero[0])):
        columna:list[str]=[]
        for j in range(len(tablero[i])):
            columna.append(tablero[j][i])
        res.append(columna)
    return res

# matriz=[[1,2,3],[4,5,6],[7,8,9]]
# print(matriz_traspuesta(matriz))

def quien_gano_el_tateti_facilito(tablero:list[list[str]])->int:
    columnas=matriz_traspuesta(tablero)
    hay_tres_X = False
    hay_tres_O = False

    for i in range(len(columnas)):
        contadorX:int=0
        contadorO:int=0        
        for jugada in columnas[i]:
            if jugada == "X":
                contadorX+=1
                contadorO=0
            elif jugada=="O":
                contadorO+=1
                contadorX=0
            else:
                contadorO=0
                contadorX=0
            
            if contadorX == 3:
                hay_tres_X = True
            if contadorO == 3:
                hay_tres_O = True

    if hay_tres_X and hay_tres_O:
        return 3
    elif hay_tres_X:
        return 1
    elif hay_tres_O:
        return 2
    else:
        return 0
    

tablero = [
    ['X', ' ', 'O', ' ', ' '],
    ['X', 'O', 'O', ' ', ' '],
    ['X', ' ', 'O', ' ', ' '],
    ['O', 'O', 'O', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ']
]


print(quien_gano_el_tateti_facilito(tablero))