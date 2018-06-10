#!/usr/bin/python3
stack = [int(x) for x in input().split()]

while stack:
  v1 = stack.pop()
  v2 = new_stack.pop()
  while v2 > v1:
    stack.append(v2)
    v2 = new_stack.pop()
  new_stack.append(v2)
  new_stack.append(v1)

print(new_stack[1:])
