#!/usr/bin/python3
import math
def d(n):
  d = 0
  for i in range(1,math.ceil(n/2+1),1):
    if n % i == 0:
      d += i
  return d

def amicable(a):
  b = d(a)
  return a == d(b) and a != b

tondeuse = 0
for i in range(0,10001):
  if amicable(i):
    tondeuse += i

print(tondeuse)
