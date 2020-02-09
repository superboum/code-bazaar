/**
 * Ce code prend un journal d'appel extrait de l'API Freebox
 * et sort un arbre graphviz des préfixes des numéros du journal.
 * L'objectif est de pouvoir identifier les spammeurs et leur préfixe.
 */

const fbx_appels = require('./calls.json')

const est_num_anonyme = numero => numero.length == 0
const est_num_normal = numero => numero.length == 10
const est_num_bizarre = numero => !(est_num_anonyme(numero) || est_num_normal(numero))

const numeros = fbx_appels.result.map(appel => appel.number)

const appels_anonymes = numeros.filter(est_num_anonyme).length
const numeros_normaux = numeros.filter(est_num_normal)
const numeros_bizarres = numeros.filter(est_num_bizarre)

const arbre_prefixes = 
  numeros_normaux
    .reduce((arbre, numero) => {
      numero.split('').reduce((bout_arbre, chiffre) => {
        if (! (chiffre in bout_arbre)) bout_arbre[chiffre] = {count:0, feuille: {}}
        bout_arbre[chiffre].count++
        return bout_arbre[chiffre].feuille
      }, arbre)
      return arbre
    }, new Object({}))

const arbre_vers_dot = (racine, bout_arbre) => {
  const feuilles = Object.keys(bout_arbre.feuille)
  feuilles.forEach(dest_clé => {
    const nouvelle_racine = racine + dest_clé
    console.log(`${racine} -> ${nouvelle_racine} [ label="${bout_arbre.feuille[dest_clé].count}" ];`)
    arbre_vers_dot(nouvelle_racine, bout_arbre.feuille[dest_clé])
  })
}

console.log("digraph G {")
arbre_vers_dot('0', { feuille: arbre_prefixes })
console.log("}")

//console.log(numeros.length, appels_anonymes, numeros_normaux.length, numeros_bizarres.length)
