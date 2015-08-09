#!/usr/bin/python3
f = open('p099_base_exp.txt', 'r').read().split("\n")
indicator = 0
index = -1
for i in range(len(f)):
  vals = f[i].split(",")
  base = int(vals[0])
  exponent = int(vals[1])
  lel = pow(base, 1/exponent) 
  if (lel > indicator):
    indicator = lel
    index = i
    print (indicator)

print(index)
