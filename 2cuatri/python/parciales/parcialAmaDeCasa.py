def mejores_precios(super1:list[tuple[str,float]],super2:list[tuple[str,float]])->list[tuple[str,float]]:
    res:list[tuple[str,float]]=[]
    for producto1 in super1:
        for producto2 in super2:
            if producto1[0] == producto2[0]:
                nombre=producto1[0]
                precio1=producto1[1]
                precio2=producto2[1]

                if precio1 < precio2:
                    res.append((nombre,precio1))
                else:
                    res.append((nombre,precio2))
                
    
    return res

# super1 = [("leche", 151.0), ("yerba", 4719.5), ("jabón",269.2)]
# super2 = [("leche", 261.2), ("yerba", 3939.1), ("jabón",319.2)]

# print(mejores_precios(super1,super2))
#2
def seguidilla(calificaciones:list[int],nota_minima:int)->int:
    contador:int=0
    l=[]
    for numero in range(len(calificaciones)-1):
        nota1=calificaciones[numero]
        nota2=calificaciones[numero+1]
        
        if nota1 >= nota_minima and nota2>=nota_minima:
            contador+=1
        
        else:
            l.append(contador)

    res=maximo(l)
    return res
    
    

def maximo(l:list[int])->int:
    res=l[0]
    for e in l:
        if e >res:
            res=e 
    return res


# calicaciones = [10,55,60,65,54,64,65,55,45,57];
# nota_minima = 70
# print(seguidilla(calicaciones,nota_minima))


#3
def elem_en_pos_pares(matriz:list[list[int]],elem:int)->list[bool]:
    res:list[bool]=[]
    for i in range(len(matriz)):
        for e in range(len(matriz[i])):
            num=matriz[i][e]
            if num == elem:
                if e%2==0:
                    res.append(True)
                else:
                    res.append(False)

    return res

#4
def pertenece(elemento,lista):
    for e in lista:
        if e == elemento:
            return True
    return False

def viajes_por_dia(viajes_diarios:dict[int,list[str]],usuarios:list[str])->dict[tuple[str,int]]:
    res:dict={}
    for nombre in usuarios:
        res[nombre]=0
    
    for llaves,valores in viajes_diarios.items():
        for nombre in usuarios:
            if pertenece(nombre,valores):
                res[nombre]+=1
    
    return res

viajes_diarios = {1 : ["Juan", "Maria"], 2 :
["Marcela","Juan"]}
usuarios = ["Juan", "Maria", "Marcela"]

print(viajes_por_dia(viajes_diarios,usuarios))