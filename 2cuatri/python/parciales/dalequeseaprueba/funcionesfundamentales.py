def matriz_traspuesta(m:list[list[int]])->list[list[int]]:
    res:list[list[int]]=[]
    for indice in range(len(m[0])):
        columna=[]
        for elemento in range(len(m)):
            columna.append(m[elemento][indice])
        res.append(columna)
    return res


def subsecuencia_mas_larga(tipos_pacientes_atendidos: list[int]) -> int:
    res:int = 0
    largo_max:int = 0
    largo_actual:int = 0
    indice:int = 0
    for t in tipos_pacientes_atendidos:
        
        if t == "perro" or t == "gato":
            largo_actual +=1
        else:
            if largo_max < largo_actual:
                res = indice - largo_actual                
                largo_max = largo_actual
            largo_actual = 0
        indice +=1

    if largo_max < largo_actual:
        res = indice - largo_actual                

    return res

def sec_larga(l:list[int])->int:
    res:int=0
    maximo:int=0
    maximo_actual:int=0
    indice:int=0
    for e in l:
        if e == "perro" or e=="gato":
            maximo_actual+=1
        else:
            if maximo < maximo_actual:
                res=indice-maximo_actual
                maximo=maximo_actual
            maximo_actual=0
        indice+=1
    
    if maximo < maximo_actual:
        res=indice-maximo_actual
    return res
