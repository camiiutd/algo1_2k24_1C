def matriz_traspuesta(m:list[list[int]])->list[list[int]]:
    res:list[list[int]]=[]
    for indice in range(len(m[0])):
        columna=[]
        for elemento in range(len(m)):
            columna.append(m[elemento][indice])
        res.append(columna)
    return res


