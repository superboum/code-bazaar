#!/usr/bin/python3

import math

def permut(n):
  length = math.factorial(n+1)
  print(length, n+1, n)
  i = 0
  while (i + 1) * (length / n+1) < 1000000:
    i += 1
  return i

print(permut(9))
