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
#let document-title = [Laboratoire\ Classification hiérarchique]

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

// Source : Steven Golovkine, Livre d'exercices, chapitre 7 (main.pdf).
// Exercices du livre : 7.1, 7.4, 7.5, 7.6, 7.27 et 7.29.
// Les énoncés sont renumérotés de 1 à 6 ; aucun corrigé n’est reproduit.
#let exercise(number, title: none, body) = {
  block(breakable: false)[
    #heading(level: 2)[
      Exercice #number
      #if title != none [ — #title]
    ]
    #body
  ]
}

// Exercice 7.1 du livre.
#exercise("1", title: "Classification ascendante hiérarchique")[
  On considère la matrice de dissimilarité entre quatre observations :

  #align(center)[
    #table(
      columns: 5,
      align: center,
      inset: 5pt,
      table.header([Observation], [$1$], [$2$], [$3$], [$4$]),
      [$1$], [$0$], [$0.30$], [$0.40$], [$0.70$],
      [$2$], [$0.30$], [$0$], [$0.50$], [$0.80$],
      [$3$], [$0.40$], [$0.50$], [$0$], [$0.45$],
      [$4$], [$0.70$], [$0.80$], [$0.45$], [$0$],
    )
  ]

  + Construire le dendrogramme obtenu par classification ascendante
    hiérarchique avec le lien simple, ou méthode du plus proche voisin. Indiquer
    la hauteur de chaque fusion.

  + Reprendre le calcul avec le lien complet, ou méthode du voisin le plus
    distant.

  + Quelle partition obtient-on en coupant chacun des deux arbres pour conserver
    exactement deux groupes ?

  + Donner, pour chaque arbre, un ordre différent des feuilles qui représente le
    même dendrogramme.
]

// Exercice 7.4 du livre.
#exercise("2", title: "CAH sur huit observations")[
  On considère la matrice de distances entre huit observations. Deux entrées ont
  été masquées par les lettres $A$ et $B$.

  #align(center)[
    #set text(size: 10pt)
    #table(
      columns: 9,
      align: center,
      inset: 3.5pt,
      table.header([Obs.], [$1$], [$2$], [$3$], [$4$], [$5$], [$6$], [$7$], [$8$]),
      [$1$], [$0$], [$1.464$], [$0.997$], [$1.346$], [$1.336$], [$1.447$], [$1.711$], [$1.785$],
      [$2$], [$1.464$], [$0$], [$1.365$], [$0.986$], [$2.761$], [$2.523$], [$0.589$], [$0.898$],
      [$3$], [$0.997$], [$1.365$], [$0$], [$1.067$], [$2.275$], [$1.708$], [$1.866$], [$1.976$],
      [$4$], [$1.346$], [$0.986$], [$1.067$], [$0$], [$2.517$], [$1.694$], [$1.528$], [$B$],
      [$5$], [$1.336$], [$A$], [$2.275$], [$2.517$], [$0$], [$1.196$], [$2.881$], [$2.034$],
      [$6$], [$1.447$], [$2.523$], [$1.708$], [$1.694$], [$1.196$], [$0$], [$2.977$], [$2.066$],
      [$7$], [$1.711$], [$0.589$], [$1.866$], [$1.528$], [$2.881$], [$2.977$], [$0$], [$0.969$],
      [$8$], [$1.785$], [$0.898$], [$1.976$], [$0.546$], [$2.034$], [$2.066$], [$0.969$], [$0$],
    )
  ]

  + Déterminer $A$ et $B$.

  + Construire la classification ascendante hiérarchique avec le lien simple,
    ou méthode du plus proche voisin. Indiquer la hauteur de chaque fusion et
    dessiner le dendrogramme.

  + Reprendre le calcul avec le lien complet, ou méthode du voisin le plus
    distant.

  + Couper chacun des deux arbres afin d'obtenir deux groupes. Donner les deux
    partitions.

  + Pour chacune de ces partitions, calculer la silhouette de l'observation
    $4$.

  + Donner un ordre différent des feuilles qui conserve l'interprétation de
    chaque dendrogramme.
]

// Exercice 7.5 du livre.
#exercise("3", title: "Comparer trois critères de liaison")[
  À une étape d'une classification ascendante, il reste trois groupes
  $A$, $B$ et $C$, chacun de taille $2$. Les dissimilarités entre paires
  d'observations appartenant à deux groupes différents sont :

  #align(center)[
    #table(
      columns: 2,
      align: center,
      inset: 5pt,
      table.header([Paire de groupes], [Dissimilarités croisées]),
      [$A,B$], [$(1,5,6,10)$],
      [$A,C$], [$(2,2.5,3.5,4)$],
      [$B,C$], [$(3,3.1,3.3,3.5)$],
    )
  ]

  + Calculer les dissimilarités entre groupes avec le lien simple, le lien
    complet et le lien moyen.

  + Quelle paire fusionne en premier selon chacun des trois critères ?

  + Une seule dissimilarité de la paire $(A,B)$ vaut $1$, tandis que les autres
    sont grandes. Expliquer pourquoi cet exemple illustre le phénomène de
    chaînage du lien simple.
]

// Exercice 7.6 du livre.
#exercise("4", title: "Critère de Ward et hausse d'inertie")[
  Trois groupes $A$, $B$ et $C$ ont les caractéristiques suivantes :

  #align(center)[
    #table(
      columns: 4,
      align: center,
      inset: 5pt,
      table.header([Groupe], [Effectif], [Centroïde], [Inertie interne]),
      [$A$], [$4$], [$(0,0)^top$], [$6$],
      [$B$], [$2$], [$(3,1)^top$], [$2$],
      [$C$], [$3$], [$(1,4)^top$], [$5$],
    )
  ]

  Pour des données euclidiennes, la hausse d'inertie provoquée par la fusion de
  deux groupes est
  $ Delta(A,B)=frac(n_A n_B, n_A+n_B) norm(mu_A-mu_B)^2. $

  + Calculer la hausse d'inertie associée à chacune des trois fusions possibles
    et indiquer celle que choisit Ward.

  + Calculer le centroïde et l'inertie interne du groupe fusionné.

  + Calculer la hausse d'inertie de la fusion ultérieure avec le troisième
    groupe.

  + Démontrer la formule de $Delta(A,B)$ à partir de la décomposition de
    l'inertie autour du centroïde fusionné.
]

// Exercice 7.27 du livre.
#exercise("5", title: "Arrestations aux États-Unis : CAH et standardisation")[
  Le fichier
  #link("https://stt2200.netlify.app/include/data/tp/USArrests.csv")[`USArrests.csv`]
  contient, pour chacun des $50$ États américains, trois taux d'arrestation et
  la proportion de population urbaine. Les variables sont `Murder`, `Assault`,
  `UrbanPop` et `Rape`.

  + Importer les données. Vérifier leurs dimensions, leur type et l'absence de
    valeurs manquantes. Comparer les échelles des quatre variables.

  + Calculer les distances euclidiennes sur les données brutes, puis effectuer
    une classification ascendante hiérarchique avec le lien simple. Tracer le
    dendrogramme et le couper en trois groupes.

  + Lister les États de chaque groupe et calculer leurs profils moyens dans les
    unités initiales.

  + Refaire toute l'analyse après centrage-réduction des quatre variables.

  + Comparer les partitions. La standardisation est-elle justifiée ? Que révèle
    la taille des groupes sur le comportement du lien simple ?
]

// Exercice 7.29 du livre.
#exercise("6", title: "Préférences musicales : classification ascendante et DIANA")[
  Le fichier
  #link("https://stt2200.netlify.app/include/data/tp/musique.csv")[`musique.csv`]
  contient neuf notes musicales `m1` à `m9` et quelques variables descriptives
  pour $47$ personnes.

  + Importer le fichier, sélectionner les neuf notes et utiliser l'identifiant
    `id` comme nom de ligne. Vérifier les valeurs manquantes et les échelles.

  + Construire une CAH en distance euclidienne avec le lien moyen. Tracer le
    dendrogramme et obtenir six groupes.

  + Construire une classification descendante avec l'algorithme DIANA du paquet
    `cluster`, puis la couper également en six groupes. DIANA ne possède pas de
    paramètre de « lien moyen » : sa règle de division est propre à l'algorithme.

  + Calculer les silhouettes globale et par groupe pour les deux partitions.
    Décrire les groupes au moyen de leurs profils moyens.

  + Comparer les deux partitions sans supposer que leurs numéros de groupe ont
    la même signification.
]
