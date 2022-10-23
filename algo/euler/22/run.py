#!/usr/bin/python3

def computeWord(w):
  v = 0
  for i in range(len(w)):
    v += ord(w[i]) - 64

  return v

f = open('p022_names.txt', 'r').read().replace("\"","").split(",")
f = sorted(f)
score = 0 
for i in  range(len(f)):
  score += (i+1)*computeWord(f[i])

print(score)
