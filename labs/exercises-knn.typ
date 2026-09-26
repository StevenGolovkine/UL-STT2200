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
#let document-title = "Laboratoire - k-NN"

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
// Les exercices sont renumérotés de 1 à 6 ; seuls les énoncés sont reproduits.
#let exercise(number, title: none, body) = {
  block(breakable: false)[
    #heading(level: 2)[
      Exercice #number
      #if title != none [ — #title]
    ]
    #body
  ]
}

// Exercice 1 de la feuille : exercice 6.8 du livre.
#exercise("1", title: "Les k plus proches voisins comme règle locale")[
  On dispose d'un échantillon $(x_i,y_i)_(i=1)^n$. Pour une distance fixée,
  $cal(N)_k (x)$ désigne les indices des $k$ observations les plus proches de
  $x$, avec $1<=k<=n$. Les égalités de distance sont départagées par l'indice
  croissant des observations.

  1. Pour une réponse qualitative à $K$ classes, définir une estimation locale
     $hat(p)_g (x)$ de #box[$P(Y=g | X=x)$] et la règle de classification associée.
  2. Montrer que le vote majoritaire minimise, parmi les classes possibles,
     le nombre d'erreurs sur les voisins sélectionnés. Que faut-il préciser
     si plusieurs classes obtiennent le même nombre de voix ?
  3. Pour une réponse quantitative, minimiser
     $sum_(i in cal(N)_k (x))(y_i-a)^2$ par rapport à $a$. Quelle prédiction
     obtient-on ? Que devient-elle avec une perte absolue ?
  4. Décrire les cas $k=1$ et $k=n$. Pourquoi parle-t-on d'une méthode
     non paramétrique ?
]

// Exercice 2 de la feuille : exercice 6.9 du livre.
#exercise("2", title: "Distances, vote et pondération des voisins")[
  On souhaite classer $x_0=(0,0)^top$ à partir des cinq observations suivantes,
  en utilisant la distance euclidienne.

  #align(center)[
    #table(
      columns: 4, align: center, inset: 4pt,
      table.header([Indice], [$x_(i 1)$], [$x_(i 2)$], [Classe]),
      [$1$], [$0.5$], [$0$], [A],
      [$2$], [$0$], [$1$], [B],
      [$3$], [$1$], [$1$], [B],
      [$4$], [$2$], [$0$], [A],
      [$5$], [$0$], [$3$], [B],
    )
  ]

  1. Calculer et ordonner les distances à $x_0$.
  2. Donner les classes prédites pour $k=1$, $3$ et $5$, avec un vote majoritaire. Pour $k=3$, calculer les deux fréquences locales de classe.
  3. Pour $k=3$, attribuer au voisin $i$ un poids proportionnel à
     $d(x_0,x_i)^(-1)$. Calculer les fréquences pondérées et la classe prédite.
  4. Expliquer comment traiter une distance nulle et une égalité de distance
     à la limite du voisinage. Ces problèmes sont-ils résolus en prenant un
     $k$ impair ?
]

// Exercice 3 de la feuille : exercice 6.12 du livre.
#exercise("3", title: "Erreur apparente et validation leave-one-out")[
  On observe six couples, ordonnés par abscisse :

  #align(center)[
    #table(
      columns: 7, align: center, inset: 4pt,
      table.header([Indice], [$1$], [$2$], [$3$], [$4$], [$5$], [$6$]),
      [$x_i$], [$0$], [$1$], [$3$], [$7$], [$8$], [$10$],
      [$y_i$], [A], [A], [B], [B], [B], [A],
    )
  ]

  On utilise la distance absolue et un vote uniforme.

  1. Calculer l'erreur du $1$-NN lorsque chaque observation peut être son propre voisin.
  2. Retirer successivement chaque observation, prédire sa classe à partir
     des cinq autres, puis calculer l'erreur _leave-one-out_ pour $k=1$ et $k=3$.
  3. Choisir $k$ parmi ces deux possibilités. Peut-on présenter le minimum
     obtenu comme une estimation sans optimisme de l'erreur du modèle choisi ?
  4. Quel problème poserait une copie exacte d'une observation répartie entre
     apprentissage et validation ?
]

// Exercice 4 de la feuille : exercice 6.26 du livre.
#exercise("4", title: "Iris : choisir k, la pondération et la standardisation")[
  Le jeu `iris`, livré avec R, contient $150$ fleurs de trois espèces et
  quatre mesures quantitatives. On cherche à prédire `Species`.

  1. Réserver aléatoirement $15$ fleurs par espèce pour le test, et utiliser
     les $35$ restantes par espèce pour l'apprentissage. Fixer une graine.
  2. Construire cinq plis stratifiés dans l'apprentissage. Comparer
     $k in {1,3,5,9,15,25}$, deux pondérations (une uniforme, une par distance) et les données brutes ou standardisées. Réestimer la standardisation dans chaque pli.
  3. Choisir la configuration de plus petite erreur de validation croisée.
     À égalité, retenir le plus grand $k$, puis la première configuration
     dans l'ordre de la grille. Représenter les erreurs en fonction de $k$.
  4. Réajuster le prétraitement retenu sur tout l'apprentissage. Évaluer
     une seule fois le modèle choisi sur le test avec une matrice de confusion,
     le taux de bonnes predictions et la moyenne des sensibilités des trois espèces.
  5. Interpréter les résultats. Pourquoi une seule partition ne suffit-elle
     pas à conclure à la supériorité générale d'une pondération ou d'une
     mise à l'échelle ?
]

// Exercice 5 de la feuille : exercice 6.27 du livre.
#exercise("5", title: "Régression k-NN et ajout de variables sans information")[
  On simule le modèle

  $ X tilde cal(U)(-1,1), quad Y=sin(pi X)+epsilon, quad
    epsilon tilde cal(N)(0,0.2^2), $

  où le bruit est indépendant de $X$. On ajoute jusqu'à vingt variables
  uniformes sur $[-1,1]$, indépendantes entre elles, de $X$ et du bruit.

  1. Générer $180$ observations d'apprentissage et $1000$ observations de
     test indépendantes. Construire trois représentations contenant $X$ seul,
     $X$ et cinq variables inutiles, puis $X$ et vingt variables inutiles.
     Conserver les mêmes réponses et observations dans les trois cas.
  2. Pour chaque représentation, choisir $k$ parmi
     $1$, $3$, $5$, $9$, $15$, $25$, $45$ et $75$ par validation croisée à
     cinq plis. Employer des poids uniformes et standardiser dans chaque pli.
  3. Évaluer sur le test la RMSE de prédiction de $Y$ et la MSE d'estimation
     de la vraie fonction $sin(pi X)$. Comparer avec la prédiction constante
     égale à la moyenne des réponses d'apprentissage.
  4. Pour la représentation contenant $X$ seul, tracer la prédiction sur
     $[-1.5,1.5]$. Décrire son comportement dans et hors du domaine observé.
  5. Expliquer pourquoi la standardisation ne suffit pas à éliminer l'effet
     des variables inutiles. Comment évaluer honnêtement une sélection de
     variables destinée à remédier au problème ?
]

// Exercice 6 de la feuille : exercice 6.28 du livre.
#exercise("6", title: "Décomposition biais-variance avec les k-plus proches voisins")[
  On souhaite illustrer empiriquement la décomposition de l'erreur quadratique
  moyenne

  $ E[(Y_0-hat(f)(x_0))^2]
    =sigma^2+"Biais"^2(hat(f)(x_0))+"Var"(hat(f)(x_0)). $

  1. Choisir une fonction de régression non linéaire $f$, une taille
     d'échantillon $n$ et une variance de bruit $sigma^2$. Générer des jeux de
     données selon
     $Y=f(X)+epsilon$, où $epsilon tilde cal(N)(0,sigma^2)$.
  2. Quel paramètre des $k$-plus proches voisins contrôle la flexibilité de
     l'estimateur ? Dans quel sens agit-il ?
  3. Fixer un unique point $x_0$. Pour plusieurs valeurs de $k$, simuler un grand
     nombre de nouveaux échantillons d'apprentissage et calculer
     $hat(f)(x_0)$ sur chacun d'eux.
  4. Estimer séparément le biais au carré, la variance, l'erreur irréductible et
     la MSE de test. Cette dernière doit être calculée directement à l'aide de
     nouvelles réponses $Y_0$, et non obtenue par addition des trois autres
     termes. Représenter les résultats et vérifier numériquement la
     décomposition.
]
