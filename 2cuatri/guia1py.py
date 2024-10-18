from math import *
# 1
def imprimir_hola_mundo():
    print("Hola Mundo")

def imprimir_un_verso():
    print("aaa.\nhola\n")

def raizDe2():
    print(round(sqrt(2)))

def factorial_2()->int:
    return 2*1

def perimetro()->float:
    return 2*pi

#2
def imprimir_saludo(nombre:str)->None:
    print("hola", nombre)
#   print("hola "+nombre)

def raiz_cuadrada_de(numero:int)->None:
    return sqrt(numero)

def fahrenheit_a_celsius(t:float)->float:
    return ((t-32)*5)/9

def imprimir_dos_veces(estribillo:str)->None:
    return estribillo+" "+estribillo

def es_multiplo_de(n:int, m:int)->bool:
    return n%m==0

def es_par(numero: int)->None:
    return es_multiplo_de(numero,2)

def cantidad_de_pizzas(comensales:int,min_cant_de_porciones:int) -> None:
    if (min_cant_de_porciones*comensales) <= 8:
        return 1 
    else:
        return round((min_cant_de_porciones*comensales)//8)

#3
def alguno_es_0(n1:float,n2:float) -> bool:
    return n1==0 or n2==0

def ambos_son_0 (n1:float,n2:float)->bool:
    return n1==0 and n2==0

def es_nombre_largo(nombre: str) ->bool:
    return 3 <= len(nombre) and len(nombre) <=8

def es_bisiesto(año: int) ->bool:
    return año%400==0 or (año%4==0 and not año%100==0)

#4
def peso_pino(pino:int) -> int:
    if pino <= 3:
        return 3*pino
    else:
        return (3*pino) + (pino-3)*2
    
def es_peso_util(peso:int) ->bool:
    return (peso_pino(peso)) >=400 and (peso_pino(peso))<=1000

def sirve_pino(pino:int)->int:
    return (es_peso_util(peso_pino(pino)))


#5
def devolver_el_doble_si_es_par(numero: int)->None:
    if numero%2==0:
        return 2*numero
    else:
        return numero 
    
def devolver_valor_si_es_par_sino_el_que_sigue(numero:int) -> None:
    if numero%2==0:
        return numero
    else: 
        return numero+1
    
def devolver_el_doble_si_es_multiplo3_el_triple_si_es_multiplo9(numero:int)->None:
    if numero%3==0:
        return numero*2
    elif numero%9==0:
        return numero*3
    else:
        return numero
    
def lindo_nombre(nombre:str) ->None:
    if len(nombre) >=5:
        return print("MUCHAS LETRAS")
    else:
        return print("T QUEDASTE CORTO")
    
def elRango(numero:int)->None:
    if len(numero) <= 5:
        return print("MENOR A 5")
    elif len(numero) >=10 and len(numero) <=20:
        return print("RANGO 10 20 PA")
    else:
        return print("MAYOR 20")
    
def jubilacion(edad:int, sexo:str) -> None:
    if edad <=18:
        return print("VACACIONES")
    elif edad >= 60 and sexo=="F":
        return print("JUBILATE")
    elif edad >=65 and sexo=="M":
        return print("JUBILATE")
    else:
        return print("A LABURAR")


#6
def dieznumeros()->None:
    i=1
    while i <=10:
        print(i)
        i+=1    #suma abreviada

#usar print solo si hay return

def numerosPares() -> None:
    i=10
    while i<=40:
        if i%2==0: 
            print(i)
        i+=1

def eco()->None:
    return "eco"*10

def viajetiempo(partida:int,llegada:int) -> None:
    i=0
    while llegada < partida:
        i+=1
        llegada+=1
        print("Viajó",i,"años estamos en el año",llegada)

def viaje_tiempo20(partida:int)->None:
    for i in range(partida,-384,-20):
        i+=1
        print("VIAJASTE 20 AÑOS, ESTAMOS EN",i)

# print(viaje_tiempo20(2024))

# 8
def ej_1()->None:
    x=5
    y=7
    return x+y

def ej_2(x:int,y:int,z:int)->bool:
    if z==x+y and y== z*2:
        return True
    else:
        return False

#me pudri