#set page(
  paper: "a4",
  margin: (x: 3cm, y: 3.35cm),
  numbering: "1",
)

#set text(font: "New Computer Modern", lang: "fr", size: 11pt)
#set par(justify: true, leading: 0.65em)
#set heading(numbering: none)
#show heading: set text(hyphenate: false)

#let course = "Université Laval, STT-2200: Analyse de données"
#let semester = "Automne 2026"
#let author = "Steven Golovkine"
#let document-title = [Laboratoire\ Méthodes d’ensemble]

#let document-header(title) = {
  align(center)[
    #text(size: 10.5pt, weight: "semibold", tracking: 0.02em)[
      #smallcaps[#course]
    ]
  ]

  v(1.05cm)
  line(length: 100%, stroke: 0.65pt)
  v(0cm)

  align(center)[
    #set par(justify: false)
    #text(size: 25pt, weight: "bold", hyphenate: false)[#title]
  ]

  v(0cm)
  line(length: 100%, stroke: 1.4pt)
  v(1.05cm)

  align(center)[
    #text(size: 13pt, weight: "semibold")[#author]
    #v(0.1cm)
    #text(size: 10.5pt)[#semester]
  ]

  v(0.3cm)
}

#document-header(document-title)

= Exercices

// Source : Steven Golovkine, Livre d’exercices, chapitre 6 (main.pdf).
// Exercices du livre : 6.28, 6.29, 6.30, 6.46 et 6.47.
// Les énoncés sont renumérotés de 1 à 5 ; aucun corrigé n’est reproduit.
#let exercise(number, title: none, body) = {
  block(breakable: false)[
    #heading(level: 2)[
      Exercice #number
      #if title != none [ — #title]
    ]
    #body
  ]
}

// Exercice 1 de la feuille : exercice 6.28 du livre.
#exercise("1", title: "Réduction de variance par bagging et forêt aléatoire")[
  On moyenne $M$ prédicteurs identiquement distribués, chacun de variance
  $sigma^2$. La corrélation entre deux prédicteurs distincts est constante et
  vaut $rho$.

  + Démontrer que la variance de leur moyenne est

    $ sigma^2 [rho+(1-rho)/M]. $

  + Calculer cette variance pour $sigma^2=1$, $rho=0.2$ et $M=100$.
  + Quelle est la limite lorsque $M$ tend vers l'infini ? À quelle condition
    peut-elle être nulle ?
  + Si les prédicteurs sont des arbres de régression, discuter l'effet d'une
    augmentation de leur profondeur sur $sigma^2$ et $rho$.
  + Dans une forêt aléatoire, seules $m$ variables parmi les $p$ disponibles
    sont candidates à chaque coupure. Expliquer le but de cette restriction
    dans la formule de variance et identifier le paramètre principalement visé.
  + Comparer les cas extrêmes $m=p$ et $m=1$, en discutant le biais des arbres
    individuels et leur corrélation.
]

// Exercice 2 de la feuille : exercice 6.29 du livre.
#exercise("2", title: "Échantillons bootstrap et erreur hors sac")[
  Un arbre de bagging est ajusté sur un échantillon bootstrap de taille $n$ tiré
  avec remise parmi $n$ observations.

  + Quelle est la probabilité qu'une observation donnée ne soit jamais tirée ?
  + Donner sa limite lorsque $n$ devient grand et la proportion asymptotique
    d'observations distinctes présentes dans un échantillon bootstrap.
  + Décrire la construction d'une prédiction hors sac (OOB) pour une observation.
  + Pourquoi l'erreur OOB est-elle utile ? Citer néanmoins une précaution.
]

// Exercice 3 de la feuille : exercice 6.30 du livre.
#exercise("3", title: "Sous-échantillonnage des variables dans une forêt")[
  Une forêt dispose de $p=100$ variables, dont cinq sont réellement informatives.
  À chaque nœud, elle tire sans remise $m=10$ variables candidates.

  + Quelle est la probabilité qu'une variable informative particulière soit
    disponible au nœud ?
  + Montrer que la probabilité qu'au moins une des cinq variables informatives
    soit disponible vaut

    $ 1-binom(95,10)/binom(100,10), $

    puis donner sa valeur approchée.
  + Décrire le compromis créé par une diminution de $m$.
  + Donner une limite de l'importance par diminution d'impureté et une limite de
    l'importance par permutation.
]

// Exercice 4 de la feuille : exercice 6.46 du livre.
#exercise("4", title: "Salaires au baseball : comparer trois ensembles")[
  Le jeu de données « Hitters » de la librairie R ISLR contient des statistiques
  de joueurs de baseball et leur salaire annuel. On souhaite prédire le
  logarithme du salaire par gradient boosting et bagging.

  1. Supprimer les observations dont le salaire est inconnu, transformer
     « Salary » par le logarithme naturel et encoder les variables qualitatives.
  2. Créer un apprentissage de $200$ observations et
     réserver les observations restantes pour le test.
  3. Pour chacune des dix valeurs
     $lambda in {10^(-2),dots,10^0}$ régulièrement espacées sur une échelle
     logarithmique, ajuster un gradient boosting de $1 000$ arbres. Tracer la
     MSE d'apprentissage en fonction de $lambda$.
  4. Tracer également la MSE de test et identifier la valeur de $lambda$ qui la
     minimise. Expliquer la réserve méthodologique créée par cette sélection.
  5. Ajuster un bagging comprenant $100$ arbres de
     profondeur maximale trois. Comparer la MSE de test à celle du gradient
     boosting et commenter la portée de la conclusion.
]

// Exercice 5 de la feuille : exercice 6.47 du livre.
#exercise("5", title: "Courriels indésirables : régler une forêt aléatoire")[
  Le fichier
  #link("https://stt2200.netlify.app/include/data/tp/spam.csv")[`spam.csv`]
  contient $57$ caractéristiques de messages et la cible `type`, qui indique
  `spam` ou `nonspam`.

  + Importer et décrire les données. Construire une partition stratifiée
    apprentissage-validation à parts égales.

  + Ajuster une forêt aléatoire avec les paramètres par défaut. Comparer erreur
    OOB, erreur de validation et erreurs par classe. Examiner les variables les
    plus importantes, sans leur attribuer une interprétation causale.

  + Choisir `mtry` par validation croisée interne sur l'apprentissage. Comparer
    au moins les valeurs $1$, $7$ et $35$, puis une grille plus fine.

  + Pour `mtry` égal à $1$, $7$ et $35$, tracer l'erreur de validation cumulée
    entre $1$ et $50$ arbres. Comment le nombre d'arbres interagit-il avec
    `mtry` ?

  + Pour `mtry = 1`, comparer sur le même graphique l'erreur OOB cumulative et
    l'erreur de validation. Pourquoi les courbes ne sont-elles pas identiques ?
]
