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
#let document-title = "Laboratoire - Analyse de Fisher"

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
// Exercices du livre : 6.1, 6.7, 6.9, 6.10, 6.13, 6.34, 6.41 et 6.42.
// Les énoncés sont renumérotés de 1 à 8 ; aucun corrigé n’est reproduit.
#let exercise(number, title: none, breakable: false, body) = {
  block(breakable: breakable)[
    #heading(level: 2)[
      Exercice #number
      #if title != none [ — #title]
    ]
    #body
  ]
}

// Exercice 6.1 du livre.
#exercise("1", breakable: true, title: "Compréhension de l'analyse discriminante")[
  On cherche à prédire l'appartenance de $80$ observations à l'un de trois
  groupes à partir de cinq variables $X_1,dots,X_5$. Les moyennes par groupe
  sont les suivantes.

  #align(center)[
    #table(
      columns: 6,
      align: center,
      inset: 4pt,
      table.header([Groupe], [$overline(X)_1$], [$overline(X)_2$],
        [$overline(X)_3$], [$overline(X)_4$], [$overline(X)_5$]),
      [$1$], [$-0.155$], [$0.202$], [$-0.003$], [$-0.909$], [$-0.952$],
      [$2$], [$0.227$], [$0.050$], [$0.091$], [$0.067$], [$0.084$],
      [$3$], [$0.505$], [$-0.313$], [$-0.092$], [$0.475$], [$0.776$],
    )
  ]

  Les matrices de sommes de carrés totale, intragroupe et intergroupe vérifient
  $S=W+B$. Trois coefficients, notés $u$, $v$ et $w$, ont été masqués :

  $ S = mat(
    62.803, u, -4.472, -0.336, 10.369;
    u, 81.039, 26.438, -4.621, 7.088;
    -4.472, 26.438, 80.627, 12.644, -6.670;
    -0.336, -4.621, 12.644, 85.557, 30.721;
    10.369, 7.088, -6.670, 30.721, 88.443
  ), $

  $ W = mat(
    56.401, 10.815, -3.671, -13.815, -6.382;
    10.815, 76.904, 25.491, 5.372, 19.919;
    -3.671, 25.491, 80.198, 13.934, -4.656;
    -13.815, 5.372, 13.934, 56.705, v;
    -6.382, 19.919, -4.656, v, 44.591
  ), $

  $ B = mat(
    6.402, -4.934, -0.800, 13.479, 16.752;
    -4.934, 4.135, 0.947, w, -12.832;
    -0.800, 0.947, 0.429, -1.290, -2.015;
    13.479, w, -1.290, 28.853, 35.364;
    16.752, -12.832, -2.015, 35.364, 43.852
  ). $

  Les deux valeurs propres non nulles de $S^(-1)B$ sont $0.751$ et $0.011$.
  Des vecteurs propres unitaires associés sont

  $ a_1=(-0.40,0.37,-0.09,-0.44,-0.71)^top, $

  $ a_2=(0.08,-0.65,-0.28,-0.64,0.29)^top. $

  + Calculer $u$, $v$ et $w$.

  + Quelle proportion du pouvoir discriminant total est portée par la première
    fonction discriminante ?

  + On classe une observation sur le premier axe en l'affectant au groupe dont
    le centre projeté $a_1^top overline(X)_k$ est le plus proche de son score.
    Dans quel groupe classe-t-on
    $x=(-0.5,0.5,0,1,1)^top$ ?
]

// Exercice 6.7 du livre.
#exercise("2", title: "Deux formulations du quotient de Fisher")[
  Soient $W$ une matrice intragroupe définie positive, $B$ une matrice
  intergroupe positive et $S=W+B$. Supposons que

  $ B a=gamma W a, quad a != 0. $

  + Montrer que $a$ est aussi vecteur propre de $S^(-1)B$ et exprimer sa valeur
    propre $lambda$ en fonction de $gamma$.
  + Donner la relation inverse et expliquer pourquoi $0<=lambda<1$.
  + Transformer les valeurs $lambda_1=0.751$ et $lambda_2=0.011$ de la question~1 en quotients de Fisher $gamma_1$ et $gamma_2$.
  + Justifier qu'il existe au plus $K-1$ axes discriminants non nuls pour $K$
    groupes.
]

// Exercice 6.9 du livre.
#exercise("3", title: "Calculer Fisher à la main et comparer aux plus proches voisins")[
  On observe les six individus suivants :

  #align(center)[
    #table(
      columns: 4, align: center, inset: 4pt,
      table.header([Indice], [$x_1$], [$x_2$], [Classe]),
      [$1$], [$0$], [$0$], [A],
      [$2$], [$2$], [$0$], [A],
      [$3$], [$1$], [$1$], [A],
      [$4$], [$3$], [$2$], [B],
      [$5$], [$5$], [$2$], [B],
      [$6$], [$4$], [$3$], [B],
    )
  ]

  1. Calculer les moyennes, la matrice intragroupe $W$ et la covariance
     commune estimée $S_p=W/(6-2)$.
  2. Déterminer une direction de Fisher dont la première coordonnée vaut $1$,
     orientée de A vers B. Calculer la valeur du quotient utilisant $W$.
  3. Avec des a priori et des coûts égaux, classer par le milieu des moyennes
     projetées. Donner le seuil et classer $x_0=(3,1)^top$.
  4. Classer le même point par $1$-NN et $3$-NN avec les distances euclidiennes
     dans les coordonnées d'origine. Expliquer les accords et les différences.
]

// Exercice 6.13 du livre.
#exercise("4", title: "Covariance singulière et direction de Fisher régularisée")[
  On dispose de $n$ observations réparties en $C$ classes non vides et de
  $p$ variables. La matrice $W$ est la somme des produits extérieurs des
  résidus centrés dans leur classe.

  1. Montrer que $"rang"(W)<=min(p,n-C)$. Donner une situation où
     $W$ est nécessairement singulière.
  2. Considérer $W="diag"(1,0)$ et $d=(1,1)^top$. Que devient le
     quotient de Fisher pour des directions tendant vers $(0,1)^top$ ?
  3. Pour $lambda>0$, remplacer $W$ par $W_lambda=W+lambda I$.
     Calculer $a_lambda=W_lambda^(-1)d$ et décrire sa direction lorsque
     $lambda$ tend vers zéro puis vers l'infini.
  4. Pourquoi une séparation parfaite dans l'échantillon ne garantit-elle
     pas une bonne généralisation ? Comment choisir $lambda$ et traiter
     les unités des variables ?
]

// Exercice 6.34 du livre.
#exercise("5", title: "Crabes : comparer LDA, k-NN et k-NN sur les axes de Fisher")[
  Le jeu `MASS::crabs` contient cinq mesures morphologiques, en millimètres,
  pour $200$ crabes. On cherche à prédire les quatre combinaisons de forme
  colorée (`sp`, bleue ou orange) et de sexe (`sex`), chacune représentée
  par $50$ observations. Les prédicteurs sont `FL`, `RW`, `CL`, `CW` et
  `BD`. Les variables `sp`, `sex` et `index` sont exclues des prédicteurs.

  1. Réserver $15$ observations par classe pour le test. Sur les $140$
     observations restantes, construire cinq plis stratifiés communs à toutes
     les méthodes. Employer des a priori LDA égaux à $1/4$.
  2. Comparer la LDA, k-NN dans les cinq variables standardisées et k-NN dans
     les $q$ premiers axes de Fisher, avec $q in {1,2,3}$.
     Pour k-NN, tester $k in {1,3,5,7,11,15,21}$ avec un vote uniforme.
  3. Réestimer la standardisation et la projection de Fisher à l'intérieur
     de chaque pli. Choisir les hyperparamètres de chaque méthode par l'erreur
     hors pli ; à égalité, préférer le plus petit $q$, puis le plus grand $k$.
  4. Réajuster les trois procédures sur l'apprentissage complet et calculer
     sur le test les matrices de confusion, le taux d'erreur et l'exactitude
     équilibrée. Représenter l'apprentissage sur les deux premiers axes.
  5. Interpréter l'effet de la projection sur les voisins. Les différences
     observées permettent-elles d'affirmer une supériorité générale ?
]

// Exercice 6.41 du livre.
#exercise("6", title: "Positions NFL : construire et appliquer l'analyse de Fisher")[
  Le fichier
  #link("https://stt2200.netlify.app/include/data/tp/nfl.csv")[`nfl.csv`]
  contient les résultats de joueurs évalués avant la sélection NFL. On
  s'intéresse aux quarts-arrières (`QB`), aux joueurs de ligne offensive (`OL`)
  et aux joueurs de ligne défensive (`DL`). Les prédicteurs sont la taille
  `Ht` et le poids `Wt`. Dans tout l'exercice, les a priori des classes
  considérées sont égaux et les erreurs ont le même coût.

  + Importer les données, examiner les valeurs manquantes puis conserver les
    observations complètes sur `Ht`, `Wt` et `PosRec`. Donner les effectifs,
    les moyennes et les écarts-types par position.

  + Pour `QB`–`OL`, construire la matrice de dispersion intragroupe $W$ et
    la covariance commune $S_p=W/(n-2)$. Calculer, sans appeler `lda`, une
    direction de Fisher proportionnelle à
    $S_p^(-1)(mu_"OL"-mu_"QB")$. La normaliser pour que $a^top S_p a=1$
    et l'orienter vers la moyenne des `OL`.

  + Définir le score centré
    $z(x)=a^top (x-(mu_"QB"+mu_"OL")/2)$ et classer en `OL` si $z(x)>0$.
    Tracer cette frontière dans le plan (`Ht`, `Wt`). Projeter un joueur
    de taille $76.5$ et de poids $335.5$, puis donner sa classe.
    Vérifier les décisions avec `MASS::lda` en fixant les mêmes a priori.
    Distinguer le critère géométrique de Fisher des hypothèses qui donnent
    une interprétation probabiliste à la LDA.

  + Programmer une validation leave-one-out en réestimant les moyennes,
    $S_p$, la direction et le seuil après chaque retrait. Donner la matrice
    de confusion pour `QB`–`OL`. Refaire le calcul pour `OL`–`DL` et
    `DL`–`QB`, puis comparer l'exactitude globale et l'exactitude équilibrée
    (moyenne des rappels des deux classes).

  + Étendre l'analyse de Fisher aux trois positions avec `MASS::lda`.
    Combien d'axes peut-on obtenir ? Vérifier que la classe prédite est le
    centroïde le plus proche dans l'espace des deux axes, avec la
    normalisation intragroupe du logiciel. Donner la confusion leave-one-out
    et expliquer pourquoi les performances binaires et multiclasses ne
    répondent pas au même problème.
]

// Exercice 6.42 du livre.
#exercise("7", title: "Positions NFL : interpréter les résultats d'une analyse de Fisher")[
  On reprend les observations complètes et les deux prédicteurs de
  l'exercice précédent. L'objectif est de produire une interprétation
  des résultats de Fisher, en distinguant séparation des groupes,
  rôle des variables et qualité des prédictions. Les a priori sont
  fixés à $1/3$ pour `DL`, `OL` et `QB` et les coûts sont égaux.

  + Avec la graine `7480`, tirer environ $70%$ des joueurs de chaque
    position pour l'apprentissage et réserver les autres pour le test.
    Respecter l'ordre des classes `DL`, `OL`, `QB` et arrondir chaque
    effectif d'apprentissage avec `round`. Ajuster l'analyse uniquement
    sur l'apprentissage ; projeter le test avec ce même modèle.

  + Calculer `fit$svd^2 / sum(fit$svd^2)`. Quelle information porte cette
    répartition entre `LD1` et `LD2` ? Est-ce une proportion de variance
    totale expliquée, comme en ACP, ou un taux de bonne classification ?

  + Examiner les coefficients bruts des axes, les coefficients multipliés
    par les écarts-types d'apprentissage et les corrélations entre variables
    et scores d'apprentissage. Interpréter chaque axe. Pourquoi les valeurs
    absolues des coefficients bruts ne suffisent-elles pas à classer
    l'importance des variables ? Que changerait une inversion du signe de
    `LD1` ?

  + Tracer les observations et les centroïdes dans le plan (`LD1`, `LD2`),
    séparément pour l'apprentissage et le test. Quelle position se distingue
    surtout sur `LD1` ? Quelles positions se chevauchent encore ?

  + Construire la matrice de confusion du test. Calculer l'exactitude
    globale, le rappel de chaque classe et leur moyenne. Comparer le
    rappel des `QB` à la précision des prédictions `QB`. Expliquer leurs
    dénominateurs respectifs.

  + Comparer les prédictions obtenues avec `LD1` seul et avec les deux axes.
    Une faible part de séparation sur `LD2` signifie-t-elle que cet axe
    est inutile ? Pourrait-on choisir le nombre d'axes sur ce test puis
    présenter le même score comme une évaluation indépendante ?

  + Pour le joueur $(76.5,335.5)$, interpréter les coordonnées, la classe
    prédite et les probabilités a posteriori. Rédiger une conclusion
    d'environ cinq lignes, avec au moins deux résultats chiffrés et une
    limite de l'analyse.
]
