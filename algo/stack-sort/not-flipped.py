#!/usr/bin/python3

# On empile sur la stack temporaire tant que le sommet de la pile originale est plus grand que le sommet de la pile temporaire
# Sinon on garde la crepe qui pose probleme puis on commence a revider la stack temporaire sur la stack originale,
# jusqu'à ce que la crepe qui pose probleme soit plus grand que le sommet de la pile temporaire,
# puis on recommence

stack = [int(x) for x in input().split()]

working = True
while working:
  # On empile 0 sur la pile temporaire
  stack_temp = [0]
  pivot = None

  # Tant que la pile originale n'est pas vide
  while stack:
    # On dépile les deux sommets
    v1 = stack.pop()
    v2 = stack_temp.pop()
    # Si sommet original plus grand que sommet temporaire,
    # Alors on empile sommet orig. sur pile temp.
    if v1 > v2:
      stack_temp.append(v2)
      stack_temp.append(v1)
    # Sinon on garde la valeur comme pivot et on sort de la boucle
    else:
      stack_temp.append(v2)
      pivot = v1
      break

  # Si on a pas de pivot, on a tout trié
  if not pivot:
    working = False

  # Tant que la pile temporaire n'est pas vide
  while stack_temp:
    v2 = stack_temp.pop()
    # Si on a un pivot ET qu'il est plus grand
    # alors on a trouvé sa place
    if pivot and v2 < pivot:
      stack.append(pivot)
      stack.append(v2)
      pivot = None
    else:
      stack.append(v2)

  stack.pop() # Remove our padding zero

print(stack)
