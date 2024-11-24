def stock_productos(stock_cambios:list[tuple[str,int]])->dict[str,tuple[int,int]]:
    res:dict={}
    for stock in stock_cambios:
        producto=stock[0]
        if not pertenece(producto,res.keys()):
            maximo=saco_maximo(stock_cambios,producto)
            minimo=saco_menor(stock_cambios,producto)
            res[producto]=(minimo,maximo)
    
    return res

def pertenece(n,l):
    for e in l:
        if e == n:
            return True
    return False

def saco_menor(l:list[tuple[str,int],],stock:str)->int:
    minimo=None
    for s in l:
        if minimo==None or stock == s[0]:
            if s[1] < minimo:
                minimo = s[1]
    return minimo

def saco_maximo(l:list[tuple[str,int]],stock:str)->int:
    maximo=None
    for s in l:
        if stock == s[0]:
            if maximo==None or s[1] > maximo:
                maximo = s[1]
    return maximo



# stock_cambios = [
#     ("manzana", 10),
#     ("pera", 5),
#     ("manzana", 15),
#     ("pera", 3),
#     ("manzana", 7),
# ]
# resultado = stock_productos(stock_cambios)
# print(resultado)  # {'manzana': (7, 15), 'pera': (3, 5)}

#2
def filtrar_codigos_primso(codigos_barra:list[int])->list[int]:
    l:list[int]=[]
    for numero in codigos_barra:
        res:int=0
        num=numero_alista(str(numero))
        if len(num) > 3:
            if pertenece(str(0),num):
                listita=[]
                n= numero_alista(str(numero%1000))
                for numerito in n:
                    if numerito!="0":
                        listita.append(numerito)
                res=sumatoria(listita)
            else:
                tres_cifras=numero%1000
                primeracifra=tres_cifras%10
                auxsegunda=tres_cifras//10
                segundacifra=int(auxsegunda%10)
                auxtercera=tres_cifras//100
                terceracifra=int(auxtercera%10)
                res+=primeracifra
                res+=segundacifra
                res+=terceracifra
        else:
            if pertenece(str(0),num):
                numeritoprima=[]
                nu=numero_alista(str(numero%1000))
                for numeritos in nu:
                    if numeritos!="0":
                        numeritoprima.append(numeritos)
                res=sumatoria(numeritoprima)
            else:
                prim=numero%10
                auxseg=numero//10
                segcifra=int(auxseg%10)
                auxter=numero//100
                tercifra=int(auxter%10)      
                res+=prim
                res+=segcifra
                res+=tercifra    

        if res%2==1:
            l.append(numero)

    return l


def numero_alista(codigo:int)->list[int]:
    l:list[int]=[]
    for dig in codigo:
        l.append(dig)
    return l

def sumatoria(l)->int:
    res=0
    for num in l:
        res+=int(num)
    return res

#3
def subsecuencia_mas_larga(tipos_pacientes_atendidos:list[str])->int:
    inicio:int=-1
    final:int=-1
    maximo:int=0
    for i in range(len(tipos_pacientes_atendidos)):
        for elemento in tipos_pacientes_atendidos:
            if elemento=="gato" or elemento=="perro":
                if inicio==-1:
                    inicio=i

                else:
                    final=i

            res=0
            res+=final
            res-=inicio

            if res > maximo:
                maximo=res
    
    return res


def subsecuencia_mas_larga(tipos_pacientes_atendidos: list[str]) -> int:
    maximo: int= 0
    longitud:int=0
    indice:int=0
    for i in range(len(tipos_pacientes_atendidos)):
        animal:str=tipos_pacientes_atendidos[i]
        if animal=="perro" or animal=="gato":
            longitud+=1
            if indice==0:
                indice=i
        else:
            if maximo > longitud:
                maximo=longitud
            indice=0

    return indice

# print(subsecuencia_mas_larga(["p", "d", "gato", "perro", "gallo","perro","perro","gato","perro"]))  

#4
def un_responsable_por_turno(grilla_horaria:list[list[str]])-> list[tuple[bool,bool]]:
    res:list=[]
    for i in range(len(grilla_horaria[0])):
        turno_manana:list[str]=[grilla_horaria[hora][i] for hora in range(0,4)]
        turno_noche:list[str]=[grilla_horaria[hora][i] for hora in range(4,8)]

        primera_tupla:bool=True
        for turno in turno_manana:
            if turno!=turno_manana[0]:
                primera_tupla=False
        
        segunda_tupla:bool=True
        for turnito in turno_noche:
            if turnito!=turno_noche[0]:
                segunda_tupla=False
        
        res.append((primera_tupla,segunda_tupla))
    return res

