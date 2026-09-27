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
#let document-title = "Laboratoire - k-moyennes"

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

// Source : Steven Golovkine, Livre d'exercices, chapitre 7 (main.pdf).
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

// Exercice 1 de la feuille : exercice 7.2 du livre.
#exercise("1", title: "Silhouette et pseudo-R²")[
  Six observations unidimensionnelles $a,b,c,d,e,f$ prennent respectivement les
  valeurs $1,2,3,4,5,6$. On utilise la distance euclidienne et la partition
  $C_1={a,b,c}$, $C_2={d,e,f}$.

  + Calculer la silhouette $s(c)$ de l'observation $c$ et l'interpréter. On note
    $a(c)$ sa distance moyenne aux autres observations de son groupe et $b(c)$
    sa plus petite distance moyenne à un autre groupe.

  + Calculer le pseudo-$R^2=1-frac(W, T, style: "horizontal")$, où
    $T=sum_i (x_i-overline(x))^2$ est l'inertie totale et
    $W=sum_k sum_(i in C_k)(x_i-overline(x)_k)^2$ l'inertie intragroupe.
]

// Exercice 2 de la feuille : exercice 7.3 du livre.
#exercise("2", title: "Itérations des k-moyennes")[
  On cherche à répartir les cinq observations suivantes en $K=2$ groupes :

  #align(center)[
    #table(
      columns: 6,
      align: center,
      inset: 5pt,
      table.header([Observation], [$1$], [$2$], [$3$], [$4$], [$5$]),
      [$x_(i 1)$], [$-1$], [$-0.5$], [$0$], [$0.5$], [$1$],
      [$x_(i 2)$], [$-1$], [$0$], [$0.5$], [$-0.5$], [$1$],
    )
  ]

  Pour rendre l'initialisation reproductible, on part de
  $C_1^((0))={1,3,5}$ et $C_2^((0))={2,4}$.

  + Calculer les deux centroïdes initiaux.

  + Calculer les distances euclidiennes au carré entre chaque observation et
    les deux centroïdes, puis l'inertie intragroupe initiale
    $W(C^((0)))$.

  + Réassigner chaque observation au centroïde le plus proche et recalculer les
    centroïdes.

  + Effectuer les itérations suivantes jusqu'à stabilisation. Donner la
    partition finale et son inertie $W$.
]

// Exercice 3 de la feuille : exercice 7.10 du livre.
// Parenthèses explicites autour du numérateur de la fraction horizontale.
#exercise("3", title: "Optimum des k-moyennes en dimension un")[
  On observe les valeurs ordonnées $0,1,2,8,9,10$ et l'on cherche $K=2$ groupes.

  + Montrer que, pour des centroïdes fixés $mu_1<mu_2$, la règle du plus proche
    centroïde produit deux intervalles séparés en $frac((mu_1+mu_2), 2, style: "horizontal")$.

  + Il suffit donc, à l'optimum, d'examiner les cinq coupures entre deux valeurs
    consécutives. Calculer l'inertie intragroupe de chacune d'elles.

  + Déterminer la partition globale optimale et ses centroïdes.

  + Que devient cette partition si toutes les observations subissent la
    transformation $x_i^star=3x_i-7$ ?
]

// Exercice 4 de la feuille : exercice 7.12 du livre.
#exercise("4", title: "Choisir le nombre de groupes")[
  Sur un même tableau, des solutions de $k$-moyennes donnent :

  #align(center)[
    #table(
      columns: 3,
      align: center,
      inset: 5pt,
      table.header([$K$], [Inertie $W_K$], [Silhouette moyenne]),
      [$1$], [$240$], [—],
      [$2$], [$120$], [$0.42$],
      [$3$], [$72$], [$0.51$],
      [$4$], [$58$], [$0.44$],
      [$5$], [$50$], [$0.39$],
    )
  ]

  + Calculer le pseudo-$R^2_K=1-frac(W_K, T, style: "horizontal")$ avec $T=W_1=240$.

  + Pourquoi la minimisation brute de $W_K$ choisirait-elle toujours le plus
    grand $K$ considéré ?

  + Quel nombre de groupes paraît ici le plus défendable ? Donner un argument
    fondé sur le coude et un autre fondé sur la silhouette.

  + Citer deux vérifications supplémentaires avant de donner une interprétation
    substantielle aux groupes.
]

// Exercice 5 de la feuille : exercice 7.14 du livre.
#exercise("5", title: "Exprimer l'inertie par les distances entre observations")[
  Soit un groupe non vide $C$ de taille $n_C$, de moyenne $overline(x)_C$.

  1. Montrer que

     $ sum_(i in C) norm(x_i-overline(x)_C)^2
       =1/(2n_C) sum_(i in C) sum_(j in C) norm(x_i-x_j)^2. $

  2. Réécrire cette identité avec une somme sur les seules paires $i<j$.
     Vérifier les deux expressions pour le groupe ${0,2,4}$.
  3. En déduire une expression du critère des k-moyennes utilisant seulement
     les distances euclidiennes entre les observations d'un même groupe.
     Pourquoi le facteur dépendant de la taille de chaque groupe est-il essentiel ?
  4. Peut-on remplacer sans précaution ces distances par n'importe quelle
     matrice de dissimilarité et conserver l'interprétation par des centroïdes
     euclidiens ?
]

// Exercice 6 de la feuille : exercice 7.33 du livre.
#exercise("6", title: "Old Faithful : régimes d'éruption et effets d'échelle")[
  Le jeu `datasets::faithful` contient $272$ observations du geyser Old
  Faithful : `eruptions` mesure la durée d'une éruption et `waiting` le
  temps d'attente jusqu'à la suivante. Les deux variables sont en minutes.

  1. Vérifier les dimensions et l'absence de valeurs manquantes. Représenter
     le nuage et comparer les écarts-types des deux variables.
  2. Standardiser les variables, puis ajuster les k-moyennes pour
     $K=1,dots,6$, avec cent démarrages par valeur de $K$. Calculer
     l'inertie et, pour $K>=2$, la silhouette moyenne. Retenir la plus grande
     silhouette, avec le plus petit $K$ en cas d'égalité.
  3. Décrire les effectifs et les moyennes des groupes retenus dans les unités
     d'origine. Numéroter les groupes par durée moyenne d'éruption croissante.
  4. Au même $K$, refaire les ajustements sur les données brutes, puis après
     conversion de la seule durée d'éruption en secondes. Comparer les
     affectations par des contingences et trois graphiques dans les mêmes
     coordonnées initiales.
  5. Pourquoi un simple changement d'unité peut-il modifier les groupes ?
     Peut-on choisir la meilleure représentation en comparant directement
     les trois inerties numériques ?
]

// Exercice 7 de la feuille : exercice 7.36 du livre.
#exercise("7", title: "Suisse : variables supplémentaires et sensibilité de la typologie")[
  Le jeu `datasets::swiss` décrit $47$ unités territoriales de Suisse
  francophone vers 1888. On utilise `Agriculture`, `Examination`, `Education`,
  `Catholic` et `Infant.Mortality` pour construire les groupes. L'indice
  `Fertility` est réservé à leur description ultérieure.

  1. Consulter `help("swiss")`, préciser le sens des cinq variables, puis
     les standardiser. Ajuster les k-moyennes avec $K=3$ et cent démarrages.
     Ce nombre de groupes est imposé pour l'exercice.
  2. Retirer successivement chacune des cinq variables et réajuster les
     k-moyennes sur les quatre autres, avec le même $K$. Comparer chaque
     partition à la référence par la proportion de paires d'observations
     dont le statut « même groupe » change.
  3. Quelle variable a le retrait le plus influent ? Examiner la contingence
     correspondante et expliquer pourquoi ce diagnostic ne dépend pas des
     numéros arbitraires des groupes.
  4. Pour la partition de référence, donner les effectifs, les unités
     territoriales et les profils moyens, en incluant maintenant `Fertility`.
     Tracer sa distribution par groupe. Pour la présentation, ordonner les
     groupes selon sa moyenne croissante, une fois la partition fixée.
  5. Pourquoi ces résultats n'établissent-ils ni un effet causal des variables
     sur la fécondité, ni une règle applicable aux individus ? Que changerait
     l'utilisation de `Fertility` pour choisir les variables ou $K$ ?
]
