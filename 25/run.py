#!/usr/bin/python3

import math

fib = []
fib.append(0)
fib.append(1)
fib.append(1)

def fibCalc(n):
  if len(fib) <= n:
    fib.append(fibCalc(n-1) + fibCalc(n-2))
  return fib[n]

i = 12
while len(str(fibCalc(i))) < 1000:
  i+=1

print(i)
