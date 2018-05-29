#!/usr/bin/pyhton3

stack = [int(x) for x in input().split()]

new_stack = []
while stack:
  v1 = stack.pop()
  while new_stack and new_stack[-1] > v1:
    stack.append(new_stack.pop())
  new_stack.append(v1)

print(new_stack)
