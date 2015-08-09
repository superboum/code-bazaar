#!/usr/bin/python3
import math
from enum import Enum

class NumType(Enum):
  Deficient = 0
  Abundant = 1
  Perfect = 2

def d(n):
  d = 0
  for i in range(1,math.ceil(n/2+1),1):
    if n % i == 0:
      d += i
  return d

def type(n):
  diff = n - d(n)
  if diff > 0:
    return NumType.Deficient
  elif diff < 0:
    return NumType.Abundant
  else:
    return NumType.Perfect

res = 0
limit = 28124
abundant = []
notASumOfTwoAbundantsNumber = 0

sumOfTwoAbudantNumbers = [False] * limit

print("Search abundant numbers")
for i in range(limit):
  if i % 1000 == 0:
    print("Progress: " + str(i/limit*100) + "%")
  if (type(i) == NumType.Abundant):
    abundant.append(i)
    #if i%2 == 1:
      #print(i)

print(str(len(abundant)) + " abundant numbers found")

print("\n\nSumming every possibilities")
for j in range(len(abundant)):
  if j % 200 == 0:
    print("Progress: " + str(j/len(abundant)*100) + "%")
  for k in range(j,len(abundant)):
    if abundant[j] + abundant[k] < limit:
      sumOfTwoAbudantNumbers[abundant[j]+abundant[k]] = True


print("\n\nSearching impossible sums with two abundant numbers")
for h in range(limit):
  if h % 1000 == 0:
    print("Progress: " + str(h/limit*100) + "%")
  if sumOfTwoAbudantNumbers[h] == False:
    res += h
    notASumOfTwoAbundantsNumber += 1

print("We have found " +str(notASumOfTwoAbundantsNumber)+ " numbers which are not a sum of two abundants.\nOnce summed, we found : " + str(res))
