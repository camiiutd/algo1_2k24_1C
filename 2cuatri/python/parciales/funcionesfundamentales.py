def matriz_traspuesta(m:list[list[int]])->list[list[int]]:
    res:list[list[int]]=[]
    for indice in range(len(m[0])):
        columna=[]
        for elemento in range(len(m)):
            columna.append(m[elemento][indice])
        res.append(columna)
    return res


def lista_de_maximos(l: list[int]) -> list[tuple[int, int]]:
    res: list[tuple[int, int]] = []
    inicio: int = -1  # Valor especial que indica "sin racha activa"

    for i in range(len(l)):
        if 0 < l[i] <= 61:  # Tiempo válido
            if inicio == -1:  # Iniciar una nueva racha
                inicio = i
        else:
            if inicio != -1:  # Fin de la racha
                res.append((inicio, i - 1))
                inicio = -1  # Reiniciar para indicar "sin racha activa"

    # Verificar si al final de la lista hay una racha válida
    if inicio != -1:
        res.append((inicio, len(l) - 1))

    return res
