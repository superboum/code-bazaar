#!/usr/bin/pyhton3

stack = [int(x) for x in input().split()]

working = True
while working:
  stack_temp = [0]
  pivot = None
  while stack:
    v1 = stack.pop()
    v2 = stack_temp.pop()
    if v1 > v2:
      stack_temp.append(v2)
      stack_temp.append(v1)
    else:
      stack_temp.append(v2)
      pivot = v1
      break

  print(stack, stack_temp, pivot)
  if not pivot:
    working = False

  while stack_temp:
    v2 = stack_temp.pop()
    if pivot and v2 < pivot:
      stack.append(pivot)
      stack.append(v2)
      pivot = None
    else:
      stack.append(v2)

  stack.pop() # Remove our padding zero

print(stack)
