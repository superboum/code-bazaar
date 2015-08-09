#!/usr/bin/python3

def computeWordValue(word):
  val = 0
  for i in range(len(word)):
    val += ord(word[i]) - 64

  return val

f = open('words.txt', 'r')
data = f.read().replace("\"","").split(",")

trian = []
dataInt = []
nextTriangleNumber = 1
nextSequenceNTriangle = 1

for j in range(300):
  if nextTriangleNumber == j:
    print(nextTriangleNumber)
    trian.append(True)
    nextSequenceNTriangle += 1
    nextTriangleNumber = 0.5 * nextSequenceNTriangle * ( nextSequenceNTriangle + 1)
  else:
    trian.append(False)

for i in range(len(data)):
    dataInt.append(computeWordValue(data[i]))

counter = 0
for k in range(len(dataInt)):
  if trian[dataInt[k]]:
    counter += 1

print(counter)
#print(computeWordValue('SKY'))
#print(data)

