#!/usr/bin/python3

import math

# x   x diag1
#  x x
#   x
#  x x
# x   x diag2

matrixSize = 1001
diag1 = list()
diag2 = list()

#Create Diag
for i in range(matrixSize):
  diag1.append(0)
  diag2.append(0)


#Intial Values
center = math.ceil(matrixSize/2)-1
diag1[center+0] = 1
diag2[center+0] = 1

#Recurrence
for j in range(1,center+1):
  diag1[center+j] = pow((j+1)*2-1,2)
  diag2[center+j] = diag1[center+j]-(6*j)
  diag1[center-j] = diag1[center+j]-(4*j)

  diag2[center-j] = diag1[center+j]-(2*j)

print(sum(diag1)+sum(diag2)-1)
