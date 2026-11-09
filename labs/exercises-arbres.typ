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
#let document-title = "Laboratoire - Arbres de décision"

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

// Source : Steven Golovkine, Livre d'exercices, chapitre 6 (main.pdf).
// Exercices du livre : 6.2, 6.24, 6.25, 6.26, 6.43 et 6.44.
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

// Exercice 6.2 du livre.
#exercise("1", title: "Construction du premier embranchement d'un arbre")[
  On cherche à prédire une réponse binaire $Y$ à partir de trois variables
  binaires. Les tableaux croisés sont :

  #grid(
    columns: (1fr, 1fr, 1fr),
    gutter: 8pt,
    table(
      columns: 3, align: center, inset: 3pt,
      table.header([], [$Y=1$], [$Y=0$]),
      [$X_1=1$], [$5$], [$5$],
      [$X_1=0$], [$5$], [$5$],
    ),
    table(
      columns: 3, align: center, inset: 3pt,
      table.header([], [$Y=1$], [$Y=0$]),
      [$X_2=1$], [$10$], [$0$],
      [$X_2=0$], [$0$], [$10$],
    ),
    table(
      columns: 3, align: center, inset: 3pt,
      table.header([], [$Y=1$], [$Y=0$]),
      [$X_3=1$], [$3$], [$9$],
      [$X_3=0$], [$7$], [$1$],
    ),
  )

  Pour un embranchement, on définit le critère après coupure comme la moyenne
  pondérée des impuretés des deux feuilles.

  + Calculer le taux d'erreur de classification après un embranchement sur
    chacune des trois variables.

  + Calculer l'indice de Gini pondéré correspondant.

  + Calculer l'entropie pondérée correspondante.

  + Quelle variable faut-il choisir pour le premier embranchement ?

  + Pour l'arbre à deux feuilles ainsi obtenu, calculer
    $R_alpha (T)=R(T)+alpha M(T)$ avec le taux d'erreur $R$, le nombre de feuilles
    $M$ et $alpha=0.5$. Comparer ce coût à celui de l'arbre réduit à sa racine.
]

// Exercice 6.24 du livre.
#exercise("2", title: "Construire manuellement un arbre de classification")[
  On observe huit individus décrits par deux variables quantitatives :

  #align(center)[
    #table(
      columns: 9,
      align: center,
      inset: 4pt,
      table.header([Observation], [$1$], [$2$], [$3$], [$4$],
        [$5$], [$6$], [$7$], [$8$]),
      [Variable 1], [$241$], [$396$], [$257$], [$259$],
        [$156$], [$193$], [$438$], [$356$],
      [Variable 2], [$96$], [$121$], [$125$], [$94$],
        [$68$], [$62$], [$133$], [$125$],
      [Classe], [A], [A], [A], [A], [B], [B], [A], [B],
    )
  ]

  Pour un nœud contenant des proportions $p_A$ et $p_B$, l'indice de Gini vaut
  $G=1-p_A^2-p_B^2$.

  1. Calculer l'indice de Gini du nœud racine.
  2. La première coupure est « Variable 1 $<300$ ». Calculer l'indice de Gini
     dans chacun des deux nœuds enfants.
  3. Calculer la diminution pondérée de l'indice de Gini produite par cette
     coupure.
  4. Dans le nœud « Variable 1 $<300$ », effectuer une seconde coupure
     « Variable 2 $<80$ ». Dessiner l'arbre obtenu et décrire la partition du
     plan $("Variable 1","Variable 2")$.
  5. Classer une nouvelle observation dont les coordonnées sont $(250,110)$.
]

// Exercice 6.25 du livre.
#exercise("3", title: "Choix d'une coupure dans un arbre de régression")[
  Six observations, déjà ordonnées selon une variable $X$, ont pour réponses

  $ y=(1,1,2,8,9,9). $

  Un arbre de régression prédit dans chaque feuille la moyenne des réponses et
  choisit la coupure qui minimise la somme des carrés résiduels (SCR).

  + Calculer la moyenne et la SCR du nœud racine.
  + Calculer la SCR après une coupure entre les troisième et quatrième
    observations.
  + Calculer la SCR après une coupure entre les deuxième et troisième
    observations.
  + Quelle coupure choisir ? Donner les deux prédictions terminales et la
    réduction de SCR obtenue.
]

// Exercice 6.26 du livre.
#exercise("4", title: "Élagage par coût-complexité")[
  Quatre sous-arbres emboîtés ont les taux d'erreur d'apprentissage et nombres de
  feuilles suivants :

  #align(center)[
    #table(
      columns: 3,
      align: center,
      inset: 5pt,
      table.header([Arbre], [$R(T)$], [$M(T)$]),
      [$T_4$], [$0.10$], [$4$],
      [$T_3$], [$0.13$], [$3$],
      [$T_2$], [$0.18$], [$2$],
      [$T_1$], [$0.32$], [$1$],
    )
  ]

  On minimise $R_alpha (T)=R(T)+alpha M(T)$.

  + Quel arbre minimise le critère pour $alpha=0.04$ ?
  + Calculer les valeurs de $alpha$ auxquelles deux arbres successifs ont le
    même coût.
  + Décrire l'arbre optimal en fonction de $alpha>=0$, en signalant les
    égalités aux points de rupture.
  + Pourquoi $alpha$ doit-il être choisi sur des données de validation ou par
    validation croisée plutôt que par l'erreur d'apprentissage seule ?
]

// Exercice 6.43 du livre.
#exercise("5", title: "Jus d'orange : ajuster et élaguer un arbre")[
  Le jeu de données « OJ » de la librarie R, ISLR, contient $1 070$ achats de
  jus d'orange. La réponse « Purchase » indique le choix entre les marques
  « CH » et « MM », et les dix-sept autres variables décrivent notamment les
  prix, les promotions et la fidélité du client.

  1. Créer, avec une graine fixée, un échantillon d'apprentissage de $800$
     observations et réserver les $270$ restantes pour le test.
  2. Ajuster un arbre de classification complet. Calculer son taux d'erreur
     d'apprentissage.
  3. Représenter l'arbre, compter ses feuilles et interpréter une feuille à
     partir de son effectif, de sa distribution de classes et de son impureté.
  4. Calculer la matrice de confusion et le taux d'erreur sur le test.
  5. Utiliser une validation croisée à cinq blocs et le chemin
     coût-complexité pour choisir le paramètre d'élagage sans consulter le test.
     Combien de feuilles possède l'arbre retenu ?
  6. Comparer les erreurs d'apprentissage et de test des arbres complet et
     élagué. Interpréter l'écart.
]

// Exercice 6.44 du livre.
#exercise("6", title: "Réclamations Rakuten : ajuster et élaguer un arbre")[
  Le fichier
  #link("https://stt2200.netlify.app/include/data/tp/rakuten.csv")[`rakuten.csv`]
  contient une cible binaire `CLAIM_TYPE` et douze variables décrivant une
  transaction et son expédition.

  + Importer les données, étudier la cible, les types de variables, les valeurs
    manquantes et les modalités rares.

  + Construire une partition aléatoire à $70%$ pour
    l'apprentissage et $30%$ pour la validation. Mettre en place une règle
    apprise sur l'entraînement pour regrouper les modalités rares et inconnues
    sous `OTHER`. Pourquoi cette étape est-elle nécessaire ?

  + Ajuster un arbre avec ses paramètres par défaut. Calculer les précisions sur le jeu d'apprentissage et le jeu de validation ainsi que le taux d'erreur du nœud numéroté $3$.

  + Ajuster un arbre maximal avec `minsplit = 2`, `minbucket = 1` et `cp = 0`.
    Comparer sa taille et ses erreurs à celles de l'arbre par défaut.

  + Élager l'arbre maximal sur une grille de valeurs de `cp`. Tracer les erreurs
    d'apprentissage et de validation en fonction de `cp`, puis choisir la
    complexité sans utiliser le jeu de validation plus d'une fois.
]
