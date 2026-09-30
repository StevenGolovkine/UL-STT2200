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
#let document-title = "Laboratoire - AFC"

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
    #text(size: 25pt, weight: "bold")[#title]
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

// Source : Steven Golovkine, Livre d'exercices, chapitre 5 (main.pdf).
// Les exercices sont renumérotés de 1 à 7 ; seuls les énoncés sont reproduits.
#let exercise(number, title: none, body) = {
  block(breakable: false)[
    #heading(level: 2)[
      Exercice #number
      #if title != none [ — #title]
    ]
    #body
  ]
}

// Exercice 1 de la feuille : exercice 5.12 du livre.
#exercise("1", title: "Survie et troubles cognitifs : une AFC")[
  Le tableau suivant donne la répartition de patients selon l'issue observée
  en ligne et l'intensité des troubles cognitifs observés en phase initiale en
  colonne.

  #align(center)[
    #table(
      columns: 5,
      align: center,
      inset: 5pt,
      table.header([], [Absence], [Troubles], [Troubles sévères], [Total]),
      [Survie], [108], [61], [13], [182],
      [Décès], [8], [4], [2], [14],
      [Total], [116], [65], [15], [196],
    )
  ]

  + Quelle méthode est la mieux adaptée à l'analyse de ce tableau ?
  + Donner le tableau des fréquences relatives.
  + Donner le profil moyen des lignes et l'interpréter.
  + Donner le profil moyen des colonnes et l'interpréter.
  + Combien d'axes factoriels non triviaux peut-on obtenir ? Justifier.
  + Quel test permet d'étudier l'indépendance entre l'issue et les troubles ?
    Sachant que l'inertie totale vaut $0.0050$, calculer la statistique de test.
]

// Exercice 2 de la feuille : exercice 5.13 du livre.
#exercise("2", title: "Profils et distance du khi carré en AFC")[
  On considère le tableau d'effectifs

  #align(center)[
    #table(
      columns: 5,
      align: center,
      inset: 4pt,
      table.header([], [$C_1$], [$C_2$], [$C_3$], [Total]),
      [$A$], [$40$], [$40$], [$20$], [$100$],
      [$B$], [$20$], [$20$], [$10$], [$50$],
      [$C$], [$45$], [$5$], [$0$], [$50$],
      [Total], [$105$], [$65$], [$30$], [$200$],
    )
  ]

  1. Calculer les masses des colonnes et les trois profils-lignes.
  2. Calculer la distance du khi carré entre les profils $A$ et $B$, puis entre
     $A$ et $C$.
  3. Comparer la distance entre $A$ et $C$ à la distance euclidienne ordinaire
     entre leurs profils. Quelle colonne reçoit relativement le plus de poids ?
  4. Pourquoi deux lignes proportionnelles représentent-elles le même point dans
     le nuage des profils, malgré des effectifs totaux différents ?
  5. Fusionner les lignes $A$ et $B$. Montrer que les masses des colonnes et la
     position de leur profil commun ne changent pas.
  6. Énoncer et interpréter le principe d'équivalence distributionnelle : que
     conserve l'AFC lorsqu'on fusionne des lignes de profils identiques ?
  7. Une distance nulle entre deux profils signifie-t-elle que les deux lignes
     ont le même effectif total ?
]

// Exercice 3 de la feuille : exercice 5.16 du livre.
#exercise("3", title: "Projeter des lignes et colonnes supplémentaires en AFC")[
  On reprend l'AFC active du tableau $2 times 2$

  $ N=mat(40,10;10,40), $

  orientée de sorte que les coordonnées standard des colonnes valent
  $Gamma=(1,-1)^top$ et celles des lignes $Phi=(1,-1)^top$. Une nouvelle ligne
  supplémentaire contient les effectifs $(30,20)$ et une nouvelle colonne
  supplémentaire les effectifs $(20,30)^top$.

  1. Calculer le profil de la ligne supplémentaire et sa coordonnée principale
     à l'aide de la formule barycentrique.
  2. Calculer le profil de la colonne supplémentaire et sa coordonnée principale.
  3. Comparer la position de la ligne supplémentaire aux coordonnées principales
     $0.6$ et $-0.6$ des deux lignes actives.
  4. Montrer qu'une ligne supplémentaire de profil $(0.1,0.9)$ aurait une
     coordonnée $-0.8$. Pourquoi un point supplémentaire peut-il sortir de
     l'intervalle occupé par les points principaux actifs ?
  5. Pourquoi ces projections ne modifient-elles pas les masses, valeurs propres
     et axes de l'analyse active ?
  6. Que se passerait-il si les nouveaux effectifs étaient ajoutés au tableau
     actif avant de refaire l'AFC ?
  7. Pourquoi la projection d'un profil fondé sur un très faible effectif doit-elle
     être accompagnée de son effectif ou d'une mesure d'incertitude ?
]

// Exercice 4 de la feuille : exercice 5.18 du livre.
#exercise("4", title: "Modalités rares et pondération du khi carré")[
  Les masses de trois colonnes sont

  $ c=(0.80,0.19,0.01). $

  Deux profils-lignes sont

  $ p=(0.50,0.49,0.01), quad q=(0.50,0.50,0). $

  1. Calculer la distance du khi carré entre $p$ et $q$ et décomposer sa valeur
     par colonne.
  2. Comparer avec deux profils qui diffèrent du même transfert de $0.01$, mais
     entre les deux premières colonnes :

     $ u=(0.49,0.51,0), quad v=(0.50,0.50,0). $

  3. Calculer le rapport des deux distances au carré et interpréter l'effet de la
     troisième colonne rare.
  4. Pourquoi ce poids élevé est-il cohérent avec l'objectif de comparer des
     profils relatifs ? Dans quelle situation devient-il instable ?
  5. Discuter trois stratégies : conserver la modalité, la fusionner avec une
     autre, ou la placer en élément supplémentaire.
  6. Pourquoi une fusion fondée uniquement sur la rareté peut-elle détruire une
     information importante ?
  7. Proposer une analyse de sensibilité et des éléments de compte rendu pour les
     modalités de très faible effectif.
]

// Exercice 5 de la feuille : exercice 5.37 du livre.
#exercise("5", title: "Écoute radio au Canada : réaliser une AFC")[
  Le fichier
  #link("https://stt2200.netlify.app/include/data/tp/radio.csv")[`radio.csv`]
  répartit l'écoute de treize types de radio entre adolescents, hommes adultes
  et femmes adultes au Canada.

  + Importer le tableau et vérifier ses dimensions, ses marges et l'absence de
    valeurs négatives. Peut-on appliquer une AFC à des durées ou des
    pourcentages qui ne sont pas des effectifs entiers ?

  + Réaliser l'AFC, donner les valeurs propres et représenter simultanément les
    profils-lignes et les profils-colonnes sur le premier plan factoriel.

  + Interpréter le premier axe à partir des coordonnées et des contributions.
    Quels types de radio caractérisent le mieux les adolescents et les adultes ?

  + Calculer les cosinus carrés des trois groupes. Commenter leur qualité de
    représentation sur le premier axe, puis sur le plan complet.
]

// Exercice 6 de la feuille : exercice 5.38 du livre.
#exercise("6", title: "Ventes de café : construire puis analyser une contingence")[
  Le fichier
  #link("https://stt2200.netlify.app/include/data/tp/coffee_sales.csv")[`coffee_sales.csv`]
  enregistre des ventes de boissons avec, notamment, le type de café et la
  période de la journée.

  + Importer les transactions, vérifier les modalités de `coffee_name` et de
    `Time_of_Day`, puis construire le tableau de contingence des effectifs.

  + Transformer ce tableau en fréquences relatives et vérifier que leur somme
    vaut $1$.

  + Effectuer l'AFC du tableau. Donner le pourcentage d'inertie expliqué par
    chaque axe, les principales contributions et une interprétation des axes.

  + Tester l'indépendance entre le type de café et la période de la journée.
    Vérifier numériquement la relation entre la statistique de Pearson et
    l'inertie totale de l'AFC.

  + Discuter la différence entre significativité statistique et importance
    descriptive de l'association.
]

// Exercice 7 de la feuille : exercice 5.39 du livre.
#exercise("7", title: "Cheveux et yeux : projeter des modalités supplémentaires")[
  Le tableau `HairEyeColor`, intégré à R, croise la couleur des cheveux, la
  couleur des yeux et le sexe de $592$ personnes. On souhaite étudier la
  relation entre les deux couleurs sans que les modalités `Red` et `Green`,
  relativement peu fréquentes, participent à la construction des axes.

  + Agréger le tableau sur le sexe afin d'obtenir une contingence cheveux
    $times$ yeux. Examiner les marges, les effectifs attendus sous indépendance
    et le résultat du test du khi carré.

  + Réaliser une AFC active après avoir retiré la ligne `Red` et la colonne
    `Green`. Donner les valeurs propres, les contributions et une
    interprétation des deux axes.

  + Projeter `Red` comme ligne supplémentaire à partir de son profil sur les
    trois colonnes actives. Projeter de même `Green` comme colonne
    supplémentaire à partir de son profil sur les trois lignes actives. Ces
    deux modalités ne doivent pas modifier les axes.

  + Refaire l'AFC avec les quatre lignes et les quatre colonnes actives.
    Comparer les inerties et les positions obtenues. Que permet d'évaluer cette
    comparaison ?

  + Construire une carte superposant les points actifs et supplémentaires.
    Rappeler pourquoi la proximité visuelle d'une ligne et d'une colonne sur
    une carte symétrique doit être interprétée avec prudence.
]
