from queue import Queue as Cola

def torneo_de_gallinas(estrategias:dict[str,str])->dict[str,int]:
    d:dict={jugador: 0 for jugador in estrategias}
    jugadores=list(estrategias.keys())
    for i in range(len(jugadores)):
        for j in range(i+1,len(jugadores)):
            jugador1=jugadores[i]
            jugador2=jugadores[j]
            accion1=estrategias[jugador1]
            accion2=estrategias[jugador2]

            if accion1=="me la banco y no me desvio" and accion2=="me la banco y no me desvio":
                d[jugador1] -=5
                d[jugador2]-=5
            elif accion1=="me desvio siempre" and accion2=="me desvio siempre":
                d[jugador1] -=10
                d[jugador2]-=10 
            elif accion1=="me la banco y no me desvio" and accion2=="me desvio siempre":
                d[jugador1]+=10
                d[jugador2]-=15
            elif accion1=="me desvio siempre" and accion2=="me la banco y no me desvio":
                d[jugador1]-=15
                d[jugador2]+=10
    
    return d

# ME_DESVIO: str = "me desvio siempre"
# NO_ME_DESVIO: str = "me la banco y no me desvio"
# d={'Juli': NO_ME_DESVIO, 'Facu': NO_ME_DESVIO, 'Lucas': ME_DESVIO, 'Ana': ME_DESVIO}
# print(torneo_de_gallinas(d))

def cola_priorizando_vips(filaClientes:Cola[str,str])->Cola[str]:
    res:Cola=Cola()
    vips=[]
    comun=[]
    restauro:Cola=Cola()
    while not filaClientes.empty():
        persona=filaClientes.get()
        restauro.put(persona)
        if persona[1]=="comun":
            comun.append(persona[0])
        elif persona[1]=="vip":
            vips.append(persona[0])
    
    for vip in vips:
        res.put(vip)
    
    for comunacho in comun:
        res.put(comunacho)
    
    while not restauro.empty():
        filaClientes.put(restauro.get())

    return res

#3
def texto_lista(texto:str)->list[str]:
    lista:list[str]=[]
    for i in texto:
        lista.append(i)
    return lista

def reversecasero(t:str)->list[str]:
    res=t[::-1]
    return res

def separo_letras(t:list[str])->list[str]:
    res:list[str]=[]
    for c in t:
        for i in c:
            res.append(i)
    return res


def cuantos_sufijos_son_palindromos(texto: str) -> int:
    contador:int=0
    texto_l:list[str]=texto_lista(texto)
    texto_dadav:list[str]=reversecasero(texto)
    texto_l_dadav:list[str]=texto_lista(texto_dadav)
    letras_sep:list[str]=separo_letras(texto_l)
    letras_sep_dadav:list[str]=separo_letras(texto_l_dadav)
    while len(letras_sep) >0:
        if letras_sep == letras_sep_dadav:
            contador+=1
        letras_sep.pop(0)
        letras_sep_dadav.pop(-1)
    return contador

#4

def columna(matriz: list[list[str]]) -> list[list[str]]:
    res: list[list[str]] = []

    for i in range(len(matriz[0])):
        l=[]
        for e in range(len(matriz)):
            l.append(matriz[e][i])
        res.append(l)
         
    return res

def quien_gano_el_tateti_facilito(tablero:list[list[str]])->int:
    ganaX=False
    ganaO=False

    col=columna(tablero)
    for c in col:
        if consecutivas(c,"X"):
            ganaX=True
        elif consecutivas(c,"O"):
            ganaO=True

    if ganaX and ganaO :
        return 3
    elif ganaX:
        return 1
    elif ganaO:
        return 2
    else:
        return 0
    
def consecutivas(lista,parametro):
    contador =0
    for i in lista:
        if i==parametro:
            contador+=1
            if contador==3:
                return True
        else:
            contador=0
    return False


tablero = [['X', 'O', ' ', ' ', ' '],
            ['X', 'X', ' ', ' ', ' '],
            ['X', ' ', 'O', ' ', ' '],
            [' ', ' ', ' ', 'X', ' '],
            [' ', ' ', ' ', ' ', 'O']]
     
print(quien_gano_el_tateti_facilito(tablero))