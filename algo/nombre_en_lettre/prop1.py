#!/usr/bin/python3
import sys

intercaler = [
  None, None, 'cent', 'mille', None, None, 'million', None, None, 'milliard', None, None
]

petits_nombres = [
  'zero', 'un', 'deux', 'trois', 'quatre', 'cinq', 'six', 'sept', 'huit', 'neuf',
  'dix', 'onze', 'douze', 'treize', 'quatorze', 'quinze', 'seize', 'dix-sept', 'dix-huit', 'dix-neuf'
]

# None = retenue -> type algebrique ou classe requise
dizaines = [
  None, None, 'vingt', 'trente', 'quarante', 'cinquante', 'soixante', None, 'quatre-vingt', None 
]

def explode_array(nbre): 
  return [x for x in nbre if x in '0123456789']

def pas_de_un_intercal(v):
  if v == ['un']: return []
  return v

def pluriel_intercal(prefix, mot):
  if prefix == ['un'] or mot == 'mille': return mot
  return mot + 's'

def generer_intercal(n, i, mot):
  prefix = pas_de_un_intercal(en_lettres(n[0:i]))
  suffix = en_lettres(n[i:])

  return prefix + [pluriel_intercal(prefix, mot)] + suffix

def nombres_intercal(n):
  for i in range(1, len(n)):
    mot = intercaler[len(n) - i]
    if mot: return generer_intercal(n, i, mot)

  return nombres_composes(n)

def nombres_composes(n):
  if len(n) < 2: return nombres_atomiques(n)
  
  carry = False
  mots = [dizaines[int(n[0])]]

  if None in mots:
    mots = [dizaines[int(n[0]) - 1]]
    carry = True
  if None in mots:
    mots = []

  return mots + nombres_atomiques(n[1:], carry)

def nombres_atomiques(n, carry=False):
  idx = int(n[0])
  if carry: idx = idx + 10
  return [petits_nombres[idx]]

def en_lettres(nbre):
  n = explode_array(nbre)
  if len(n) > len(intercaler): raise Exception('Number is too big')

  return nombres_intercal(n)

for line in sys.stdin:
  print('-'.join(en_lettres(line)))
