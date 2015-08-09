#!/usr/bin/python3

class Node:
  op = 0
  opPrint = 100000
  routes = 0
  def __init__(self,v):
    self.parent = None
    self.children = list()
    self.value = v
    self.subtotal = -1
  
  def addChild(self, n):
    n.parent = self
    self.children.append(n)
   
  def getMaxValue(self):
    if self.subtotal == -1:
      maxV = 0
      self.addOperation()
      for i in range(len(self.children)):
        tempV = self.children[i].getMaxValue() 
        if tempV > maxV:
          maxV = tempV

      self.subtotal = self.value + maxV
    return self.subtotal

  def addOperation(self):
    Node.op += 1
    if self.op % self.opPrint == 0:
      print("Ootch, already "+str(Node.op)+ " operation made... ")
    if len(self.children) == 0:
      Node.routes += 1

  def getBiggerChild(self):
    maxV = -1
    selectedNode = None
    for i in range(len(self.children)):
      if maxV < self.children[i].value:
        maxV = self.children[i].value
        selectedNode = self.children[i]

    return selectedNode

  def __repr__(self):
    return 'Node '+str(self.value)


f = open('p067_triangle.txt','r').read().split("\n")
root = Node(int(f[0]))

nodes = []

#Create Node
for i in range(len(f)-1):
  nodes.append(list())
  curLine = f[i].split(" ")
  for j in range(len(curLine)):
    n = Node(int(curLine[j]))
    nodes[i].append(n)

#Linking

for k in range(len(nodes)-1):
  for l in range(len(nodes[k])):
    nodes[k][l].addChild(nodes[k+1][l])
    nodes[k][l].addChild(nodes[k+1][l+1])

print("Max Value : " + str(nodes[0][0].getMaxValue()))
print("I have made " + str(Node.op) + " operations")
print("Routes tested " + str(Node.routes))
