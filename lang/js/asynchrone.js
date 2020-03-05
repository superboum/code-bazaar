const fs = require('fs')
const section = titre => (...texte) => console.log("■■■■■■", titre, "■", ...texte)

/********************************************************************************************************/
/********************************************************************************************************/
const affiche1 = section("1. utilisation des callbacks")

/*
 * signature de notre fonction (syntaxe non contractuelle) :
 *   fs.readFile(file: string, encoding: string, callback: function(err: error, data: string):void):void
 * La fonction utilise l'abastraction callback pour l'asynchrone
 * La fonction retourne tout de suite
 * Au moment où les données seront lues, le callback sera appelé
 */

fs.readFile('/proc/loadavg', 'utf-8', (err, res) => err ? affiche1(err) : affiche1(res.trim()))

/********************************************************************************************************/
/********************************************************************************************************/
const affiche2 = section("2. explication du callback hell")

/*
 * Maintenant, le moment où ça devient embêtant, c'est quand il faut enchaine plusieurs résultats.
 * Prenons l'exemple où l'on veut vérifier la charge CPU du système, si elle est faible on s'autorise à aller voir les infos du CPU.
 * On est donc obligé d'attendre le résultat de la load avant de regarder les infos du CPU
 */

fs.readFile('/proc/loadavg', 'utf-8', (err, loadavg) => {
  if (err) {
    affiche2(`Impossible de récupérer la charge. ${error}`)
    return
  } 
  const chargeCPU = parseFloat(loadavg)
  if (chargeCPU > 4.0) {
    affiche2(`Charge CPU trop élevée (${chargeCPU}), on ne fait rien.`)
    return
  }

  affiche2(`Charge CPU OK (${chargeCPU}). On inspecte la machine`)
  fs.readFile('/proc/sys/kernel/hostname', 'utf-8', (err, hostname) => {
    if (err) {
      affiche2(`Impossible de récupérer les infos du CPU. ${error}`)
      return
    }
    /* /!\ EN OPTIMISANT AU MAX ON EST DEJA A 2 NIVEAUX D'INDENTATION /!\ */
    affiche2(`Nom de la machine: ${hostname.trim()}`)
  })
})

/*
 * CONCLUSION : ON SE RETROUVE AVEC UN "CALLBACK HELL"
 * Des callbacks qui s'imbriquent dans des callbacks qui s'imbriquent dans des callbacks, etc.
 */

/********************************************************************************************************/
/********************************************************************************************************/
const affiche3 = section("3. utiliser les promesses")
/*
 * En réalité, fs.readFile a aussi une version promesse : fs.promises.readFile !
 * Réécrivons notre code précédent avec !
 */

/*
  Créer un objet 'Promise'                      Plus de callback
     VVV                                              VVV          */
const p = fs.promises.readFile('/proc/loadavg', 'utf-8')

/*
On rajoute la fonction a exécuter via .then
VVV                                               */
p.then(loadavg => {
    const chargeCPU = parseFloat(loadavg)
    if (chargeCPU < 4.0) {
      affiche3(`La charge système est faible (${chargeCPU}). On s'autorise donc à inspecter le système.`)
      /* 
         On créer une nouvelle promesse ici mais on ne lui met pas de .then, on la retourne et elle va être automatiquement chainée plus bas
             VVVVVVVVVVVVVVVVVVVV                                                                       */
      return fs.promises.readFile('/proc/sys/kernel/hostname', 'utf-8')
    } else {
      affiche3(`Le système est surchargé (${chargeCPU}), ne lançons pas d'inspection inutile.`)
      return null;
    }
  })
/*
Le 'callback' de notre deuxième 'Promise' est ici
Il est au même niveau que le callback de la promesse 1
Plus d'imbrication à l'infini !!!
  VVVVV                                                              */
  .then(hostname => {
    if (hostname === null) return
    affiche3(`Nom de la machine: ${hostname.trim()}`)
  })
  .catch(erreur => affiche3(`Erreur: ${erreur}`))


/********************************************************************************************************/
/********************************************************************************************************/
const affiche4 = section("4. convertir des fonctions utilisant les callbacks vers des promesses")

/*
 * Le temps d'un instant, oublions l'existence de fs.promises.readFile
 * Nous voudrions donc créer un objet promesse à partir de fs.readFile !
 * C'est facile, on va l'appeler fsprom ! Commençons de manière verbeuse...
 */

function fsprom(file, encode) {
  return new Promise(function (resolve, reject) {
    fs.readFile(file, encode, function (err, res) {
      if (err != null) reject(err)
      else resolve(res)
    })
  })
}

/*
 * et avec du sucre syntaxique, c'est plus joli mais ça fait pareil :
 */

const fsprom2 = (file, encode) => 
  new Promise((resolve, reject) => 
    fs.readFile(file, encode, (err, res) => err ? reject(err) : resolve(res)))

/*
 * On teste de nouveau !
 */

/*
Changement ici
VVVVVVV                                            */
fsprom2('/proc/loadavg', 'utf-8')
  .then(loadavg => {
    const chargeCPU = parseFloat(loadavg)
    if (chargeCPU < 4.0) {
      affiche4(`La charge système est faible (${chargeCPU}). On s'autorise donc à inspecter le système.`)
/*
          Changement ici
             VVVVVVV                                            */
      return fsprom2('/proc/sys/kernel/hostname', 'utf-8')
    } else {
      affiche4(`Le système est surchargé (${chargeCPU}), ne lançons pas d'inspection inutile.`)
      return null;
    }
  })
  .then(hostname => {
    if (hostname === null) return
    affiche4(`Nom de la machine: ${hostname.trim()}`)
  })
  .catch(erreur => affiche4(`Erreur: ${erreur}`))

/********************************************************************************************************/
/********************************************************************************************************/
const affiche5 = section("5. écrire soit même l'abstraction 'Promesse' !")

/*
 * Javascript propose deja un objet Promise mais nous allons créer le notre, pour comprendre comment Callback et Promise sont deux abstractions équivalentes
 * Notre implémentation 'Promesse' respecte partiellement l'implémentation 'Promise' de Node (pas de finally, pas de support du throw dans les résout/rejette)
 */

class Promesse {
  constructor(execution) {
    this.résout = []
    this.rejette = null

    execution(
      succes => {
        // On vérifie qu'on a un callback à appeler (qu'on a un appel de 'ensuite' sur notre promesse)
        if (this.résout.length === 0) return 

        // On appelle le premier "ensuite" et on transforme son résultat en promesse si besoin
        const prochain_ensuite = this.résout.shift() // On enlève le premier élément de la liste
        const resultat_du_ensuite = prochain_ensuite(succes)
        const prochaine_promesse = 
          resultat_du_ensuite instanceof Promesse ?
            resultat_du_ensuite :
            new Promesse((resolve, reject) => resolve(resultat_du_ensuite))

        // On transfert nos ensuite à la nouvelle promesse
        prochaine_promesse.ensuite(...this.résout)
        prochaine_promesse.attrape(this.rejette)
      },
      echec => this.rejette(echec)
    )
  }

  ensuite(...nouveaux) {
    this.résout.push(...nouveaux)
    return this
  }

  attrape(rejette) {
    this.rejette = rejette
    return this
  }
}

/*
 * On convertit fs.readFile non plus vers les "Promise" de node
 * Mais vers notre nouvel objet "Promesse" fait maison !
 */

const fsprom3 = (file, encode) => 
  new Promesse((resolve, reject) => 
    fs.readFile(file, encode, (err, res) => err ? reject(err) : resolve(res)))

/*
 * on vérifie que ça marche :
 */

fsprom3('/proc/loadavg', 'utf-8')
  .ensuite(loadavg => {
    const chargeCPU = parseFloat(loadavg)
    if (chargeCPU < 4.0) {
      affiche5(`La charge système est faible (${chargeCPU}). On s'autorise donc à inspecter le système.`)
      return fsprom3('/proc/sys/kernel/hostname', 'utf-8')
    } else {
      affiche5(`Le système est surchargé (${chargeCPU}), ne lançons pas d'inspection inutile.`)
      return null;
    }
  })
  .ensuite(hostname => {
    if (hostname === null) return
    affiche5(`Nom de la machine : ${hostname.trim()}`)
  })
  .attrape(erreur => affiche5(erreur))

/*
 * À venir : comment passer des promesses aux générateurs, et en quoi async/await est du sucre syntaxique sur les générateurs
 */
