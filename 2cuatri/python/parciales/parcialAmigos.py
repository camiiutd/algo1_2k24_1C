def promedio_de_salidas(registro: dict[str, list[int]]) -> dict[str, tuple[int, float]]:
    res:dict={}
    for persona in registro:
        primera_tupla:int=promedio(registro[persona])
        seg_tupla:float=saco_promedio(primera_tupla,registro[persona])
        res[persona]=(primera_tupla,seg_tupla)
    return res

def saco_promedio(n:int,l:list[int])->float:
    suma:int=len(l)
    res= n/suma
    return res

def promedio(l:list[int])-> int:
    res:int=0
    for e in l:
        if e>0 and e<=61:
            res+=1
    return res


def tiempo_mas_rapido(tiempos_salas:list[int])->int:
    posicion:int=-1
    minimo:int=tiempos_salas[0]
    res=0
    for tiempo in tiempos_salas:
        posicion+=1
        if (tiempo< minimo and tiempo > 0 and tiempo <61):
            minimo=tiempo
            res=posicion
        
    return res

# def racha_mas_larga(tiempos:list[int])->tuple[int,int]:
#     for indice in range(len(tiempos)):
#         l:list=[]
#         tiempo=tiempos[indice]
#         guardo_anterior:int=tiempos[indice]
#         cc:int=0
#         if tiempo <61 and tiempo >0:
#             l.append(tiempo)
#         if tiempo < guardo_anterior:
#             tiempo=guardo_anterior
        
#         if len(l) >
            

def escape_en_solitario(amigos_por_salas:list[list[int]])->list[int]:
    l:list[int]=[]
    for indice in range(len(amigos_por_salas)):
        amigos_de_sala = amigos_por_salas[indice]
        primer_amigo = amigos_de_sala[0] == 0
        segundo_amigo = amigos_de_sala[1] == 0
        tercer_amigo = amigos_de_sala[2] != 0
        cuarto_amigo = amigos_de_sala[3] == 0
        if (primer_amigo and segundo_amigo and tercer_amigo and cuarto_amigo):
            l.append(indice)
        
    return l

a=([[0,0,1,0],
[0,0,61,0],
[0,0,0,0],
[0,0,60,0]])

print(escape_en_solitario(a))