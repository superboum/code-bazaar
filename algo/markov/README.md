# Markov Chain

Un essai sur les chaines de Markov.
Pour l'instant, on génère surtout du charabia.

## Design

 - Le dataset est composé d'une dizaine de textes de Marx traduits en français
 - On regarde les 3 mots ou signe de ponctuation précédents, appelés tokens ici, pour prédire le mot suivant
 - On initialise au hasard avec une suite de 3 tokens connus
 - Si il y a plusieurs valeurs possibles pour une suite de 3 tokens possibles, on prend au hasard
 - La "chaîne de Markov" est stocké sous forme d'une table de hachage qui revient à `(prev1, prev2, prev3) -> [(next1, occ1), (next2, occ2), ..]`

## Run

```bash
scheme markov.scm
```

Ensuite:

```scheme
(demo1)
```

## Exemple

>  Pour acheter une telle force, le travailleur parcellaire devient même d'équivalent exigent maintenant un examen plus approfondi. 
Ce qui aujourd'hui devenue indispensable. La révolution précipitait dans une succession rapide tous les anciens partis d'opposition plus avancée, c'étaient celles d'une production historique déterminée. Les quantités de valeur. Examinons donc maintenant la production au lieu de force de travail vivante. Il est très vrai. La perte de valeur relative de chaque objet en comparaison avec les autres dans la société postérieurement à la division sociale du travail suppose l'échange pour autant qu'ils obtenaient le même salaire quotidien, hebdomadaire, etc .

## Dataset

Textes de Marx traduits en Français, extrait de Wikisource
