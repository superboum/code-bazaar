#!/usr/bin/python3
from decimal import *
import math

length = 10000
getcontext().prec = length
fmt = "{:.10000f}"

maxSerieSize = 1
maxSerieI = 0

for i in range(1,1000):
  dec = str(fmt.format(Decimal(1)/Decimal(i)))[2:]
  for size in range(1,math.ceil(length/2)):
    isASerie = True
    for j in range(0,length-size,size):
      #print(dec[0:size])
      #print(dec[j:size+j])
      if dec[0:size] != dec[j:j+size]:
        isASerie = False
        break

    if isASerie:
      if maxSerieSize < size:
        print("Size: "+str(size) + " - Divider: "+str(i))
        maxSerieSize = size
        maxSerieI = i
      break

print(maxSerieI)
