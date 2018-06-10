#!/usr/bin/pyhton3

stack = [int(x) for x in input().split()]

new_stack = []
while stack:
  # 1. On prend le sommet de la pile originale
  top_stack = stack.pop()

  # 2. Tant que notre pile temporaire n'est pas vide
  while new_stack:
    # 2.1 On prend le sommet de la pile temporaire
    top_new_stack = new_stack.pop()

    # 2.2 On compare les deux sommets
    # Si le sommet de la pile courante est plus petit que le sommet de la pile temporaire,
    # on empile le sommet de la pile temporaire sur la pile courante
    if top_new_stack > top_stack:
      stack.append(top_new_stack)

    # Sinon on rempile le sommet de la pile temporaire sur la pile temporaire
    # et on arrête la boucle
    else:
      new_stack.append(top_new_stack)
      break

  # 3. On empile le sommet de la pile originale sur la pile temporaire
  # de sorte qu'on empile toujours des valeurs de plus en plus grandes sur la
  # pile temporaire
  new_stack.append(top_stack)

print(new_stack)
