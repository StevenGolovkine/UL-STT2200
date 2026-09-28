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
#let document-title = "Laboratoire - ACP"

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
// Les exercices sont renumérotés de 1 à 8 ; seuls les énoncés sont reproduits.
#let exercise(number, title: none, body) = {
  block(breakable: false)[
    #heading(level: 2)[
      Exercice #number
      #if title != none [ — #title]
    ]
    #body
  ]
}

// Exercice 1 de la feuille : exercice 5.3 du livre.
#exercise("1", title: "Compréhension de l'ACP")[
  Soit le vecteur aléatoire
  $X = (X_1, X_2, X_3, X_4)^top$, d'espérance $mu$ et de matrice de
  variance-covariance $Sigma$, avec

  $ mu = mat(0; 1; -1; 0), quad
    Sigma = mat(
      9, 1, -1, 2;
      1, 4, -1, 1;
      -1, -1, 16, 0;
      2, 1, 0, 9
    ). $

  Les valeurs propres de $Sigma$ et des vecteurs propres unitaires associés
  sont, à l'arrondi près,

  $ lambda_1 = 16.27, quad alpha_1 = (0.165, 0.098, -0.980, 0.059)^top, $
  $ lambda_2 = 11.12, quad alpha_2 = (0.665, 0.169, 0.171, 0.707)^top, $
  $ lambda_3 = 6.95,  quad alpha_3 = (0.718, -0.017, 0.077, -0.691)^top, $
  $ lambda_4 = 3.67,  quad alpha_4 = (0.118, -0.981, -0.070, 0.139)^top. $

  + Trouver une combinaison linéaire de $X_1, X_2, X_3$ et $X_4$ dont la
    variance vaut $6.95$.

  + On pose
    $Z = a_0 + a_1 X_1 + a_2 X_2 + a_3 X_3 + a_4 X_4$, avec
    $sum_(j=1)^4 a_j^2 = 1$. Déterminer les coefficients pour que $Z$ soit
    centrée et de variance maximale. Donner cette variance et la covariance
    entre $Z$ et la combinaison trouvée à la question précédente.

  + Donner une matrice diagonale $M$ telle que $M Sigma M = R$, où $R$ est la
    matrice de corrélation de $X$.
]

// Exercice 2 de la feuille : exercice 5.5 du livre.
#exercise("2", title: "Reconstruction d'une matrice de covariance")[
  Soit $X = (X_1,X_2,X_3)^top$ un vecteur aléatoire de matrice de
  variance-covariance $Sigma$. Ses valeurs propres sont $3$, $2$ et $1$, et
  des vecteurs propres unitaires associés sont respectivement

  $ v_1 = (0,-1/sqrt(2),1/sqrt(2))^top, quad
    v_2 = (1,0,0)^top, quad
    v_3 = (0,1/sqrt(2),1/sqrt(2))^top. $

  Les variables $X_1$, $X_2$ et $X_3$ représentent respectivement la
  circonférence du poignet droit, un score de capacité pulmonaire et l'indice
  de masse corporelle.

  + Les médecins souhaitent construire un score
    $C = c_1 X_1 + c_2 X_2 + c_3 X_3$ de variance maximale sous la contrainte
    $sum_(j=1)^3 c_j^2=1$. Déterminer les coefficients et la variance maximale.

  + Quelle proportion de la variabilité totale est capturée par $C$ ?

  + Retrouver $Sigma$ à partir de sa décomposition spectrale.
]

// Exercice 3 de la feuille : exercice 5.6 du livre.
#exercise("3", title: "Scores, corrélations et qualité de représentation en ACP")[
  Trois variables centrées et réduites ont pour matrice de corrélation

  $ R=mat(1,0.8,0;0.8,1,0;0,0,1). $

  Ses valeurs propres décroissantes et des vecteurs propres unitaires associés
  sont

  $ lambda_1=1.8, quad a_1=1/sqrt(2)(1,1,0)^top, $

  $ lambda_2=1, quad a_2=(0,0,1)^top, $

  $ lambda_3=0.2, quad a_3=1/sqrt(2)(1,-1,0)^top. $

  On note $Y_k=a_k^top X$ la composante principale $k$.

  1. Écrire explicitement les trois composantes principales et vérifier que
     leur somme de variances vaut la variance totale.
  2. Montrer que, pour une ACP sur variables réduites,

     $ "Corr"(X_j,Y_k)=sqrt(lambda_k) a_(j k). $

  3. Calculer les coordonnées des trois variables dans le cercle des
     corrélations sur les axes $1$ et $2$.
  4. On définit la contribution de la variable $j$ à l'axe $k$ par
     $a_(j k)^2$. Calculer les contributions aux deux premiers axes et vérifier
     qu'elles somment à $1$ sur chaque axe.
  5. Calculer le cosinus carré de chaque variable avec le premier plan, défini
     comme la somme des carrés de ses corrélations avec $Y_1$ et $Y_2$.
  6. Pour l'individu centré et réduit $z=(1,-1,2)^top$, calculer ses trois
     scores, sa reconstruction sur les deux premiers axes et son erreur
     quadratique de reconstruction.
  7. Que devient l'ensemble de ces résultats si l'on remplace $a_1$ par
     $-a_1$ ?
]

// Exercice 4 de la feuille : exercice 5.8 du livre.
#exercise("4", title: "Individus et variables supplémentaires en ACP")[
  Une ACP a été ajustée sur les trois variables actives centrées et réduites de
  l'exercice 3. Une variable
  quantitative supplémentaire standardisée $W$ possède avec les variables
  actives le vecteur de corrélations

  $ r_W=(0.6,0.6,0)^top. $

  Elle n'a pas participé au calcul des axes.

  1. Expliquer pourquoi l'ajout de $W$ comme variable supplémentaire ne modifie
     ni les vecteurs propres ni les valeurs propres de l'ACP active.
  2. Montrer que sa corrélation avec la composante $Y_k$ vaut

     $ "Corr"(W,Y_k)=frac(r_W^top a_k,sqrt(lambda_k)), $

     puis calculer ses coordonnées sur les trois axes.
  3. Les moyennes des variables actives sont $(10,20,30)$ et leurs écarts-types
     $(2,5,10)$. Projeter comme individu supplémentaire
     $x_("sup")=(12,15,50)^top$.
  4. Pourquoi faut-il employer les moyennes, écarts-types et axes de l'analyse
     initiale plutôt que les recalculer après l'arrivée de cet individu ?
  5. Comment projeter un individu dont une variable active est manquante ?
     Pourquoi n'existe-t-il pas de projection exacte sans convention
     supplémentaire ?
  6. Une variable qualitative supplémentaire décrit trois groupes. Que peut-on
     représenter sur le plan factoriel sans rendre l'ACP supervisée ?
  7. Que changerait l'inclusion de $W$ parmi les variables actives ?
]

// Exercice 5 de la feuille : exercice 5.11 du livre.
#exercise("5", title: "Combien de composantes conserver ?")[
  Une ACP centrée et réduite de $p=8$ variables produit les valeurs propres

  $ (3.1,1.6,1.0,0.8,0.6,0.4,0.3,0.2). $

  1. Calculer les proportions de variance expliquée et leurs cumuls.
  2. Combien d'axes faut-il conserver pour atteindre au moins $80%$ de variance
     expliquée ?
  3. Appliquer le critère de Kaiser. Discuter la convention pour une valeur
     propre exactement égale à $1$.
  4. Une analyse parallèle fournit comme valeurs propres moyennes sous le modèle
     nul

     $ (1.35,1.22,1.13,1.05,0.98,0.91,0.84,0.77). $

     Combien d'axes suggère-t-elle de retenir ?
  5. Calculer l'erreur minimale de reconstruction, exprimée comme variance
     résiduelle totale, lorsque l'on conserve deux puis quatre axes.
  6. Pourquoi les critères de variance cumulée, de Kaiser, d'éboulis et
     d'analyse parallèle peuvent-ils conduire à des choix différents ?
  7. Adapter le critère de choix aux objectifs suivants : visualisation,
     compression, débruitage et prédiction d'une réponse externe.
]

// Exercice 6 de la feuille : exercice 5.31 du livre.
#exercise("6", title: "ACP de caractéristiques musicales Spotify")[
  Le fichier `spotify.csv` fourni avec le devoir contient $1000$ morceaux et dix
  genres musicaux. Chaque ligne comprend l'artiste, le titre, le genre et neuf
  caractéristiques numériques :

  - `danceability`, `energy`, `loudness` et `speechiness` ;
  - `acousticness`, `instrumentalness` et `liveness` ;
  - `valence` et `tempo`.

  Les variables comprises entre $0$ et $1$ sont des indices audio ; `loudness`
  est mesurée en décibels et `tempo` en battements par minute.

  1. Importer les données, vérifier les dimensions, les types et les valeurs
     manquantes. Justifier une ACP centrée et réduite sur les neuf variables.
  2. Ajuster l'ACP et donner la proportion de variance expliquée par chacun des
     trois premiers axes, puis leur cumul.
  3. Tracer l'éboulis des valeurs propres et le premier plan factoriel. Colorer
     les morceaux selon `track_genre`, sans utiliser cette variable pour ajuster
     les axes.
  4. À partir des coefficients des deux premiers axes, proposer une
     interprétation. Rappeler pourquoi le signe d'un axe n'a pas de sens propre.
  5. Un nouveau morceau possède, dans l'ordre des neuf variables,

     $ (0.676,0.885,-8.482,0.100,0.0169,0.694,0.0909,0.0954,145)^top. $

     Le projeter sur les trois premiers axes en réutilisant exactement les
     moyennes et écarts-types de l'ACP.
  6. Comparer ses coordonnées aux centroïdes des dix genres dans l'espace des
     trois premiers axes. Proposer un genre plausible et expliquer pourquoi ce
     résultat n'est pas une validation supervisée.
]

// Exercice 7 de la feuille : exercice 5.33 du livre.
#exercise("7", title: "Évaluer la stabilité d'une ACP par bootstrap")[
  Le jeu `USArrests` comporte quatre variables pour $50$ États américains.

  1. Réaliser une ACP centrée et réduite et conserver ses deux premières
     directions comme référence.
  2. Tirer $1000$ échantillons bootstrap de $50$ lignes. Recalculer entièrement
     la standardisation et l'ACP dans chaque échantillon.
  3. Aligner le signe de chaque direction bootstrap sur la direction de
     référence, puis construire des intervalles empiriques à $95%$ pour les
     coefficients des deux premiers axes.
  4. Calculer, pour chaque réplication, le plus grand angle principal entre les
     sous-espaces bidimensionnels de référence et bootstrap. Pourquoi cette
     quantité est-elle insensible aux signes et aux rotations internes ?
  5. Réaliser une analyse laisser-un-État-dehors. Identifier les États dont le
     retrait fait le plus tourner le premier axe.
  6. Comparer l'incertitude des axes individuels à celle du premier plan.
  7. Pourquoi ce bootstrap constitue-t-il surtout une étude de sensibilité, et
     non nécessairement une inférence vers une population aléatoire d'États ?
]

// Exercice 8 de la feuille : exercice 5.36 du livre.
#exercise("8", title: "Coquillages : conduire et interpréter une ACP")[
  Le jeu de données
  #link("https://stt2200.netlify.app/include/data/tp/abalone.csv")[`abalone.csv`]
  contient $4 177$ coquillages. Pour chaque coquillage, on dispose du genre,
  de sept mesures physiques et du nombre d'anneaux, utilisé comme indicateur de
  l'âge.

  + Importer les données. Identifier les variables quantitatives et
    qualitatives, rechercher les valeurs manquantes et produire, pour chaque
    variable quantitative, la moyenne, l'écart-type, le minimum et le maximum.
    Repérer les valeurs qui méritent une vérification.

  + Réaliser une ACP centrée et réduite sur les huit variables quantitatives.
    Le genre sera conservé comme variable qualitative supplémentaire : il ne
    doit pas participer à la construction des axes.

  + Comparer trois règles de choix du nombre d'axes : critère de Kaiser, coude
    de l'éboulis et seuils de variance expliquée de $90%$ et $95%$.

  + Calculer les contributions des variables aux trois premiers axes. Quelles
    sont les deux variables qui contribuent le plus au deuxième axe ?

  + Calculer les contributions des individus. Quelles sont les deux
    observations qui contribuent le plus au troisième axe ? Revenir aux données
    brutes pour comprendre pourquoi.

  + Interpréter le premier axe et calculer la proportion de variance expliquée
    par les trois premiers axes. Comparer également les coordonnées moyennes des
    trois genres sur les deux premiers axes.

  + Expliquer pourquoi la standardisation est, ou non, pertinente dans ce
    contexte. Comparer brièvement avec une ACP seulement centrée.
]
