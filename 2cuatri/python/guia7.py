def pertenece(s:list[int],e:int)->bool:
    if e in s:
        return True
    else:
        return False
    

def divide_a_todos(s:list[int],e:int)->bool:
    for elemento in s:
        if elemento%e==0:
            return True
        else:
            return False
        
def suma_total(s:list[int])->int:
    res:int=0
    for elemento in s:
        res+=elemento
    return res


def maximo(s:list[int])->int:
    maximo:int=0
    for elemento in s:
        if elemento>=maximo:
            maximo=elemento
        else:
            continue
    return maximo

def minimo(s:list[int])-> int:
    minimo:int=0
    for ele in s:
        if ele <=minimo:
            minimo=ele
        else:
            continue
    return minimo
