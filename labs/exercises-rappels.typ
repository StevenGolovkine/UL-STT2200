#set page(
  paper: "a4",
  margin: (x: 3cm, y: 3.35cm),
  numbering: "1",
)

#set text(font: "New Computer Modern", lang: "fr", size: 11pt)
#set par(justify: true, leading: 0.65em)
#set heading(numbering: none)

#let course = "Université Laval, STT-2200: Analyse de données"
#let semester = "Automne 2026"
#let author = "Steven Golovkine"
#let document-title = "Laboratoire - Rappels"

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

#let exercise-counter = counter("exercise")

#let exercise(title: none, body) = {
  exercise-counter.step()
  heading(level: 2)[
    Exercice #context exercise-counter.display()
    #if title != none [ — #title]
  ]
  body
}

// Dupliquer ce bloc pour ajouter d'autres exercices.
#exercise(title: "Combinaisons linéaires et matrice de covariance")[
  Un vecteur aléatoire $(X,Y)^top$ a pour moyenne
  $mu=(2,-1)^top$ et pour matrice de covariance

  $ Sigma=mat(4,1.5;1.5,9). $

  1. Vérifier que $Sigma$ est définie positive.
  2. Calculer la corrélation entre $X$ et $Y$.
  3. Pour $Z=2X-Y$, déterminer $E(Z)$ et $"Var"(Z)$ à l'aide d'un produit
     matriciel.
  4. Pour $W=X+Y$, calculer $E(W)$, $"Var"(W)$ et $"Cov"(Z,W)$.
  5. Peut-on conclure que $Z$ et $W$ sont indépendantes si leur covariance est
     nulle ? Préciser un cas où cette conclusion serait valide.
]

#exercise(title: "Paradoxe des deux enfants")[
  Chaque enfant est supposé indépendamment fille ou garçon avec probabilité
  $frac(1, 2, style: "horizontal")$. L'ordre aîné-cadet est distingué.

  1. Une famille a deux enfants et l'aînée est une fille. Quelle est la
     probabilité que la cadette soit aussi une fille ?
  2. Une autre famille répond « oui » à la question « avez-vous au moins un garçon ? ». Quelle est la probabilité qu'elle ait deux garçons ?
  3. On choisit au hasard l'un des deux enfants d'une troisième famille et l'on apprend que cet enfant est un garçon. Quelle est la probabilité que l'autre enfant soit aussi un garçon ? Pourquoi cette expérience diffère-t-elle de la question 2 ?
  4. Un parent est rencontré avec l'une de ses filles, qui indique être née un vendredi. Sous le protocole où l'enfant rencontrée est identifiée, quelle est la probabilité que l'autre enfant soit une fille ?
  5. Comparer la question précédente au protocole abstrait : « la famille a au moins une fille née un vendredi ». Calculer alors la probabilité d'avoir deux filles, en supposant les sept jours équiprobables et indépendants du sexe.
  6. Formuler la leçon méthodologique générale de ces variantes.
]

#exercise(title: "Dépistage, prévalence et formule de Bayes")[
  Une affection touche $2%$ d'une population. Un test a une sensibilité de
  $90%$ et une spécificité de $95%$. On note $D$ l'affection et $+$ un résultat
  positif.

  1. Construire un arbre de probabilités ou un tableau attendu pour
     $10 000$ personnes.
  2. Calculer $P(+)$ et la valeur prédictive positive $P(D|+)$.
  3. Calculer la valeur prédictive négative $P(overline(D)|-)$.
  4. Expliquer pourquoi une sensibilité et une spécificité élevées peuvent
     coexister avec une valeur prédictive positive modeste.
  5. Deux tests sont réalisés et leurs erreurs sont supposées indépendantes
     conditionnellement au statut $D$. Calculer $P(D|+,+)$.
  6. Pourquoi l'hypothèse d'indépendance conditionnelle doit-elle être discutée
     si le même appareil et le même échantillon biologique sont réutilisés ?
]

#exercise(title: "Estimation par Monte-Carlo")[
  Le carré $[-1,1]^2$ a une aire égale à $4$ et le disque unité qu'il contient a
  une aire égale à $pi$. Si $(X,Y)$ est uniforme sur le carré, alors

  $ P(X^2+Y^2 <= 1)=pi/4. $

  1. Écrire une fonction `estimate_pi(n)` qui simule $n$ points et renvoie
     $hat(pi)=frac(4 sum_(i=1)^n bold(1){X_i^2+Y_i^2 <= 1}, n, style: "horizontal") $.
  2. Avec une graine fixée, simuler $100 000$ points. Représenter un
     sous-échantillon en colorant les points selon leur appartenance au disque.
  3. Calculer l'estimation courante
     $hat(pi)(n)=frac(4 sum_(i=1)^n bold(1){X_i^2+Y_i^2 <= 1}, n, style: "horizontal")$ pour $n=1,dots,100 000$. Tracer $abs(hat(pi)(n)-pi)$ pour une grille logarithmique
     de tailles.
  4. Répéter $1000$ fois l'expérience avec $n=2000$. Comparer la moyenne et
     l'écart-type empiriques des estimations à la théorie.
  5. Construire un intervalle de confiance normal approximatif à $95%$ pour
     $pi$. Discuter sa validité lorsque $n$ est petit.
]

#exercise(title: "Loi des gaz parfaits")[
  Dans un récipient fermé, le volume $V$ et la quantité de matière $n$ sont
  constants. La loi des gaz parfaits, $P V=n R T$, prédit donc une relation
  linéaire entre la pression $P$ et la température $T$. Les températures sont en
  kelvins et les pressions en kilopascals.

  ```r
  temperature <- c(
    406, 296, 272, 449, 483, 439, 460, 276, 321, 462,
    408, 322, 285, 411, 491, 359, 453, 486, 413, 350,
    263, 456, 390, 462, 389, 494, 303, 496, 336, 460
  )

  pressure <- c(
    1365, 982, 898, 1486, 1596, 1481, 1506, 906, 1085, 1542,
    1367, 1072, 955, 1379, 1633, 1186, 1499, 1606, 1378, 1156,
    867, 1514, 1306, 1525, 1287, 1665, 1020, 1635, 1118, 1529
  )
  ```

  1. Tracer la pression en fonction de la température.
  2. Construire $X=(bold(1)|T)$ et $Y=P$. Calculer
     $hat(beta)=(X^top X)^(-1)X^top Y$.
  3. Retrouver le résultat avec la fonction `lm` de R, puis calculer les résidus et $R^2$.
  4. Interpréter $hat(beta)_0$ et $hat(beta)_1$ physiquement. Quelle valeur
     théorique attend-on pour l'ordonnée à l'origine ?
  5. Le volume vaut $10 " dm"^3$ et
     $R=8.314 " J mol"^(-1) " K"^(-1)$. Estimer la
     quantité de matière, en prêtant attention à la conversion des kPa en Pa.
  6. Produire un graphique des résidus en fonction de la température. Quelles
     limites empêchent de déclarer la loi physique « prouvée » par ce seul
     ajustement ?
]

#exercise(title: "Loi d'Ohm, covariance et ajustement linéaire")[
  Une résistance est soumise à plusieurs intensités $A$ et la tension $V$ est
  mesurée à ses bornes :

  #align(center)[
    #table(
      columns: 8,
      align: center,
      inset: 4pt,
      table.header([Mesure], [$1$], [$2$], [$3$], [$4$], [$5$], [$6$], [$7$]),
      [Intensité (A)], [$0.2$], [$0.5$], [$0.9$], [$1.0$], [$1.2$], [$1.3$], [$1.8$],
      [Tension (V)], [$4.0$], [$10.4$], [$18.7$], [$21.1$], [$25.1$], [$27.4$], [$37.8$],
    )
  ]

  Dans les questions 1 à 3, utiliser les moments empiriques avec diviseur $n$.

  1. Calculer les moyennes $overline(A)$ et $overline(V)$.
  2. Calculer les variances de $A$ et $V$, puis leur covariance.
  3. En déduire la corrélation empirique. Que suggère-t-elle et que ne
     prouve-t-elle pas ?
  4. Ajuster le modèle $V=beta_0+R A+epsilon$ par moindres carrés, avec
     $R=frac("Cov"(A,V), "Var"(A), style: "horizontal")$, puis calculer $beta_0$.
  5. La loi physique idéale impose $beta_0=0$. Estimer alors $R$ directement et
     comparer les deux pentes.
  6. Examiner les rapports $frac(V_i, A_i, style: "horizontal")$ et les résidus du modèle avec constante.
     Pourquoi les valeurs ne sont-elles pas exactement identiques ?
]

#exercise(title: "Loi des grands nombres et théorème central limite")[
    Soient $X_1,dots,X_n$ indépendantes de loi exponentielle de taux $2$. Ainsi,
  $mu=E(X_i)=0.5$ et $sigma=0.5$. On étudie

  $ Z_n=sqrt(n) (overline(X)_n-mu)/sigma. $

  1. Écrire une fonction qui simule $B$ réalisations de $Z_n$ sans boucle sur
     les répétitions.
  2. Pour $B=5000$ et $n in {5,30,100}$, superposer à l'histogramme de $Z_n$ la
     densité normale standard.
  3. Comparer moyenne, variance et quantiles empiriques de $Z_n$ aux valeurs
     normales théoriques.
  4. Pour chaque $n$, estimer la probabilité de couverture de l'intervalle

     $ overline(X)_n plus.minus 1.96 sigma/sqrt(n). $

  5. Refaire la question précédente en remplaçant $sigma$ par l'écart-type
     empirique de chaque échantillon. Commenter l'effet de $n$ et de
     l'asymétrie de la loi exponentielle.
]
