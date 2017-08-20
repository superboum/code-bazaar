#!/usr/bin/python3

import re,copy,sys

class Sudoku:
  allSteps = 0
  stepsToVictory = 0
  routes = 0

  def __init__(self,mat):
    self.matrix = mat
    self.way = list()
    self.name = "No Name"
    self.size = 9

  def loadMatrix(self, rawMatrix):  
    self.matrix = list()
    raw = rawMatrix.split("\n")
    for s in range(len(raw)):
      self.matrix.append(list())
      for t in range(len(raw[s])):
        self.matrix[s].append(int(raw[s][t]))
    self.matrix.pop() #Dirty, sry :'(

  def __str__(self):
    s = ""
    for i in range(self.size):
      if i % 3 == 0:
        s += "-------------------------\n"
      for j in range(self.size):
        if j % 3 == 0:
          s += "| "
        if self.matrix[i][j] != 0:
          s += str(self.matrix[i][j]) + " "
        else:
          s += ". "

      s += "\n"

    return s

  def possibilities(self,x,y):

    pos = list(range(1,10))
    pos = self.possibilitiesLine(x,y,pos)
    pos = self.possibilitiesColumn(x,y,pos)
    pos = self.possibilitiesSquare(x,y,pos)
    return pos

  def reset():
    Sudoku.allSteps = 0
    Sudoku.stepsToVictory = 0
    Sudoku.routes = 0

  def stats():
    return "Every steps: "+str(Sudoku.allSteps) + " --- Steps to victory: "+str(Sudoku.stepsToVictory) + " --- Routes tried: "+str(Sudoku.routes)

  def possibilitiesLine(self,x,y, pos):
    for i in range(self.size):
      if self.matrix[x][i] in pos:
        pos.pop(pos.index(self.matrix[x][i]))

    return pos
  def possibilitiesColumn(self,x,y, pos):
    for i in range(self.size):
      if self.matrix[i][y] in pos:
        pos.pop(pos.index(self.matrix[i][y]))

    return pos

  def possibilitiesSquare(self,x,y, pos):
    xTop = x - x%3
    yTop = y - y%3
    for i in range(xTop,xTop+3):
      for j in range(yTop,yTop+3):
        if self.matrix[i][j] in pos:
          pos.pop(pos.index(self.matrix[i][j]))
    return pos

  def findAGoodCoordinate(self):
    c = [-1,-1]
    minPos = 10
    for i in range(0,9):
      for j in range(0,9):
        if self.matrix[i][j] == 0:
          pos = len(self.possibilities(i,j))
          if pos < minPos:
            c = [i,j]
            minPos = pos

    return c

  def play(self,x,y,n):
    self.matrix[x][y] = n

  def getEulerSolution(self):
    return  str(self.matrix[0][0]) + str(self.matrix[0][1]) + str(self.matrix[0][2])

  # @return : a list of step to win this sudoku
  def resolve(self):
    c = self.findAGoodCoordinate()
    Sudoku.allSteps += 1
    
    #Cas d'arret succes : plus de case vide
    if c[0] == -1:       
      Sudoku.stepsToVictory += 1
      Sudoku.routes += 1
      return self
    
    #Cas d'arret echec : des cases vides mais plus de solution
    tries = self.possibilities(c[0],c[1])
    if len(tries) == 0:
      Sudoku.routes += 1
      return None 
    
    # Récursion
    for h in range(len(tries)):
      newSudo = copy.deepcopy(self)
      newSudo.play(c[0],c[1],tries[h])
      winner = newSudo.resolve()
      if winner != None:
        Sudoku.stepsToVictory += 1
        return winner
    return None



l = (re.compile("Grid [0-9]*\n").split(sys.stdin.read()))[1:]

print(str(len(l)) + " sudokus loaded")

#print(s)
#print(s.possibilities(4,4))
res = 0

for i in range(len(l)):
#for i in range(7):
  print("Grid "+str(i))
  s = Sudoku(None)
  s.loadMatrix(l[i])
  s = s.resolve()
  print(s)
  res += int(s.getEulerSolution())


print(Sudoku.stats())
print("Euleur's answer: "+str(res))
