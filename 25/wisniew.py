# -*- coding: utf-8 -*-

"""

Created on Sun Mar 22 00:21:16 2015



@author: Thomas

"""

from math import floor,log10
def fib():
    a, b = 0, 1
    while 1:
        yield a
        a, b = b, a + b


def problem():
    a=fib()
    next(a)
    i=1
    while floor(log10(next(a)))+1<1000:
        i+=1
    return(i)

print(problem()) 
