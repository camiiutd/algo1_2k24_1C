#pilas
#1
from queue import LifoQueue
import numpy as np


def generar_nros_al_azar(cantidad:int,desde:int,hasta:int)->LifoQueue[int]:
    p=LifoQueue()
    while cantidad > 0:
        p.put(np.random.randint(desde,hasta))
        cantidad-=1
    return p


