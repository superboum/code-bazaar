#!/usr/bin/python3
import sys
from functools import reduce

def pipeline(init, *fn):
  val = init
  for f in fn:
    val = f(val)
  return val

def explode_array(nbre): 
  return [int(x) for x in nbre if x in '0123456789']

def etiquette_chiffres(n):
  size = len(n)
  return [('chiffre', x, size - i) for i, x in enumerate(n)]

def est_un(g, element):
  genre, *reste = element
  return genre == g

def intercalable(n, pos, nom):
   def ajouter_nom(acc, element):
     if not est_un('chiffre', element): return acc + [element]
     genre, valeur, index = element
     if index != pos: return acc + [element]
     return acc + [element, (nom,)]

   return reduce(ajouter_nom, n, [])

def milliard(n):
  return intercalable(n, 10, 'milliard')

def million(n):
  return intercalable(n, 7, 'million')

def mille(n):
  return intercalable(n, 4, 'mille')

def cent(n):
  def ajouter_cent(acc, element):
    if not est_un('chiffre', element): return acc + [element]
    genre, valeur, index = element

    # nombre magique, retravailler cette partie
    if index % 3 != 0: return acc + [element]
    return acc + [element, ('cent',)]
  
  return reduce(ajouter_cent, n, [])

def compose_ou_atomique(n):
  def ajouter_info(acc, element):
    precedent, res = acc
    if not est_un('chiffre', element):
      return ('autre', res + [element])

    genre, valeur, idx = element
    nouveau_genre = 'compose' if precedent == 'chiffre' else 'atomique'
    return ('chiffre', res + [(nouveau_genre, valeur, idx)])
    
  precedent, res = reduce(ajouter_info, reversed(n), ('autre', []))
  return list(reversed(res))

def retenue(n):
  dizaines_composees = [1, 7, 9]
  def ajouter_retenue(acc, element):
    info, res = acc
    if est_un('compose', element):
      genre, valeur, *reste = element
      new_info = valeur in dizaines_composees
      return (new_info, res + [element])
    if est_un('atomique', element):
      etiquette = 'retenue' if info else 'pas_de_retenue'
      return (False, res + [element + (etiquette,)])

    return (info, res + [element])

  info, res = reduce(ajouter_retenue, n, (False, []))
  return res

def generer_mots_atomique(n):
  mots = [
    'zero', 'un', 'deux', 'trois', 'quatre', 'cinq', 'six', 'sept', 'huit', 'neuf',
    'dix', 'onze', 'douze', 'treize', 'quatorze', 'quinze', 'seize', 'dix-sept', 'dix-huit', 'dix-neuf'
  ]

  def ajouter_mot(element):
    if not est_un('atomique', element): return element
    genre, val, idx, retient = element
    if retient == 'retenue': val += 10 # critiquable ?
    return ('mot', mots[val])
 
  return [ajouter_mot(element) for element in n]

def generer_mots_compose(n):
  mots = [
    None, None, 'vingt', 'trente', 'quarante', 'cinquante', 'soixante', 'soixante', 'quatre-vingt', 'quatre-vingt'
  ]
  
  def ajouter_mots(acc, element):
    if not est_un('compose', element): return acc + [element]
    genre, val, idx = element
    if not mots[val]: return acc
    return acc + [('mot', mots[val])]

  return reduce(ajouter_mots, n, [])

def generer_mots_restants(n):
  def ajouter_mots(element):
    if est_un('mot', element): return element
    genre, *reste = element
    return ('mot', genre)

  return [ajouter_mots(x) for x in n]

def tirets(n): 
  # pas propre
  return '-'.join([val for genre, val in n])

def pas_de_un_devant_intercalle(n):
  # TODO
  return n

def enlever_zero(n):
  # TODO
  return n

def regle_du_et(n):
  # TODO
  return n

def en_lettres(v):
  return pipeline(
    v,
    explode_array,
    etiquette_chiffres,
    milliard, million, mille, cent,
    compose_ou_atomique,
    retenue,
    generer_mots_atomique,
    generer_mots_compose,
    generer_mots_restants,
    tirets
  )

for line in sys.stdin:
  print(en_lettres(line))

