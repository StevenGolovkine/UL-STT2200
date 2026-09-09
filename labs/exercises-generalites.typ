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
#let document-title = "Laboratoire - Généralités"

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

// Source : Steven Golovkine, Livre d'exercices, chapitre 4 (main.pdf).
// Les numéros du livre sont conservés ; seuls les énoncés sont reproduits.
#let exercise(number, title: none, body) = {
  block(breakable: false)[
    #heading(level: 2)[
      Exercice #number
      #if title != none [ — #title]
    ]
    #body
  ]
}

#exercise("1", title: "Identifier la tâche, l'espace et la géométrie")[
  Pour chacun des scénarios suivants :

  - préciser l'unité statistique et le nombre d'observations ;
  - proposer un problème de classification ou de régression, puis de prédiction ou d'inférence ;
  - décrire l'espace des variables explicatives du problème précédent;
  - proposer une dissimilarité ou distance et le prétraitement qu'elle exige ;
  - signaler une difficulté que la formulation initiale ne résout pas.

  1. Cinq hôpitaux recrutent chacun $100$ patients. On observe l'âge, l'indice de masse corporelle, le statut vaccinal et l'apparition d'une complication respiratoire.
  2. Une base contient $2000$ ventes immobilières avec prix, superficie, nombre de chambres et présence d'un garage.
  3. Pendant $365$ jours, on relève la concentration de PM2.5, la température, le vent et l'humidité dans une ville.
  4. Un sondage interroge $2000$ personnes sur leur satisfaction de $1$ à $5$, leur âge, leur province, leur revenu et leur niveau d'éducation.
  5. On veut reconnaître quatre espèces animales à partir de trois mesures morphologiques relevées sur $1000$ animaux.
]

#exercise("2", title: "Du mandat au plan d'analyse de données")[
  Un service d'urgence dispose de $10000$ visites. La base contient les heures d'arrivée, de triage, de première consultation et de sortie, le niveau de gravité, l'âge, le mode d'arrivée, l'occupation du service, les effectifs en poste et un retour éventuel dans les $72$ heures. La direction demande de « trouver les causes de l'attente et construire un outil pour mieux planifier le personnel ».

  1. Décomposer cette demande en une question descriptive, une question explicative ou causale et une question prédictive. Préciser la décision associée à chacune.
  2. Définir la population cible, l'unité statistique et une variable réponse pour chaque question.
  3. Pour la question causale, proposer un effet à estimer. Indiquer au moins trois obstacles empêchant une interprétation causale immédiate.
  4. Pour la question prédictive, dresser la liste des variables disponibles au moment de la prédiction. Repérer les fuites d'information possibles.
  5. Choisir un protocole de validation et des critères d'évaluation compatibles avec l'usage opérationnel.
  6. Rédiger le squelette du plan d'analyse : analyses principales, analyses de sensibilité, diagnostics, livrables et critères d'acceptation.
  7. Expliquer pourquoi un seul modèle ne répond pas nécessairement aux trois questions.
]


#exercise("3", title: "Choisir et calculer des distances")[
  1. Un robot se déplace librement du point $(0,0)$ au point $(8,6)$. Calculer la
     distance euclidienne.
  2. Sur une grille, un taxi va de l'intersection $(5,42)$ à $(1,114)$. Calculer
     la distance de Manhattan en unités de pâtés de maisons.
  3. Un entrepôt est en $(10,15)$, le client A en $(18,22)$ et le client B en
     $(5,8)$. Comparer les proximités euclidienne et de Manhattan.
  4. Calculer la distance de Hamming entre les messages `1001` et `1101`.
  5. Trois utilisateurs ont aimé les ensembles de films suivants :

     $ U_1={"Titanic","Avatar","Star Wars","Matrix","Inception"}, $

     $ U_2={"Avatar","Matrix","Batman","Superman","Inception"}, $

     $ U_3={"Inception","Avengers","Spiderman","Superman","Batman"}. $

     Calculer les distances de Hamming sur l'univers total des films, puis les
     distances de Jaccard. Identifier les paires les plus proches.
  6. Expliquer pourquoi le choix entre ces distances ne peut pas être séparé de
     la définition de l'espace et des unités.
]

#exercise("4", title: "Distance de Gower pour des données mixtes")[
  Trois clients sont décrits par :

  #align(center)[
    #table(
      columns: 5,
      align: center,
      inset: 4pt,
      table.header([Client], [Âge], [Revenu], [Fumeur], [Satisfaction]),
      [A], [$20$], [$30$], [non], [$4$],
      [B], [$50$], [$60$], [oui], [$2$],
      [C], [$35$], [$45$], [non], [$5$],
    )
  ]

  Le revenu est exprimé dans une même unité pour tous. Pour chaque variable
  quantitative ou ordinale, on divise l'écart absolu par l'étendue observée. Une
  variable nominale contribue $0$ en cas d'égalité et $1$ sinon. La distance de
  Gower est la moyenne des contributions disponibles.

  1. Calculer les étendues des trois variables ordonnées.
  2. Déterminer les quatre contributions puis la distance pour chaque paire.
  3. Identifier les clients les plus proches.
  4. Recalculer $d(A,C)$ si le revenu de C est manquant.
  5. Discuter l'effet d'une valeur de revenu extrême sur toutes les distances.
  6. Proposer deux pondérations si la satisfaction est jugée deux fois plus
     importante que chacune des autres variables.
]

#exercise("5", title: "Échelles, pondérations et changement de voisin")[
  Une observation cible et deux observations candidates sont décrites par l'âge
  en années et le revenu annuel en dollars :

  $ Q=(50,50000), quad A=(51,80000), quad B=(60,52000). $

  1. Calculer les distances euclidiennes brutes de $Q$ à $A$ et à $B$. Quel est
     le plus proche voisin ?
  2. Normaliser l'âge par une étendue de $20$ ans et le revenu par une étendue de
     $100000$ dollars, puis refaire le calcul. Que constate-t-on ?
  3. Pour la distance normalisée pondérée

     $ d_w^2(x,y)=w_a (frac(x_a-y_a,20))^2
       +w_r (frac(x_r-y_r,100000))^2, $

     déterminer la condition sur le rapport $w_a/w_r$ pour que $A$ soit plus
     proche que $B$.
  4. Expliquer pourquoi le choix des unités, de la transformation et des poids
     est une hypothèse sur la similarité, et non une opération neutre.
  5. Une variable nominale à $m$ modalités est remplacée par $m$ indicatrices.
     Quelle est la distance euclidienne entre deux modalités différentes ?
     Discuter le poids implicite de cette variable parmi d'autres prédicteurs.
  6. Dans une étude prédictive, sur quelles observations faut-il estimer les
     paramètres de standardisation ?
]

#exercise("6", title: "Décomposition biais-variance de l'erreur prédictive")[
  À une valeur fixée $x_0$, on observe

  $ Y=f(x_0)+epsilon, quad E(epsilon)=0, quad "Var"(epsilon)=sigma^2. $

  Un algorithme entraîné sur un échantillon aléatoire produit
  $hat(f)(x_0)$. Le nouvel écart $epsilon$ est indépendant de l'échantillon
  d'entraînement.

  1. Démontrer la décomposition

     $ E[(Y-hat(f)(x_0))^2]
       =sigma^2+"Biais"(hat(f)(x_0))^2+"Var"(hat(f)(x_0)). $

  2. Interpréter chacun des trois termes.
  3. Trois procédures ont les caractéristiques suivantes, avec $sigma^2=2$ :

     #align(center)[
       #table(
         columns: 3,
         align: center,
         inset: 4pt,
         table.header([Procédure], [Biais au carré], [Variance]),
         [A], [$4$], [$0.5$],
         [B], [$1$], [$1.5$],
         [C], [$0.1$], [$5$],
       )
     ]

     Calculer l'erreur prédictive attendue et choisir la procédure.
  4. Pourquoi la procédure la plus flexible peut-elle avoir la plus faible
     erreur d'entraînement et une mauvaise erreur future ?
  5. Comment la taille de l'échantillon agit-elle généralement sur la variance ?
]

#exercise("7", title: "Sélection de modèle, validation et fuites d'information")[
  Une équipe dispose de $1000$ observations indépendantes et veut comparer
  $20$ configurations. Elle prévoit $60%$ pour l'apprentissage, $20%$ pour la
  validation et $20%$ pour le test final. Certaines variables sont manquantes et
  doivent être imputées, puis standardisées.

  1. Donner les tailles des trois sous-échantillons et le rôle de chacun.
  2. Décrire l'ordre correct de l'imputation, de la standardisation, de
     l'ajustement et de l'évaluation.
  3. Expliquer la fuite créée si les moyennes d'imputation et les écarts-types
     sont estimés sur les $1000$ observations.
  4. Pourquoi ne faut-il pas choisir la meilleure des $20$ configurations sur
     le jeu de test ?
  5. Proposer une validation croisée qui utilise mieux les $800$ observations
     non réservées au test.
  6. Adapter le protocole si les données sont temporelles ou regroupées par
     patient.
  7. Après l'ouverture du test, dans quel cas peut-on annoncer honnêtement sa
     performance comme évaluation finale ?
]

#exercise("8", title: [Construire et exécuter un plan exploratoire avec `airquality`])[
  Le jeu `airquality`, fourni avec R, contient des mesures quotidiennes de
  qualité de l'air et de météorologie à New York entre mai et septembre 1973.
  L'objectif est de produire une analyse exploratoire reproductible sans choisir
  les graphiques uniquement après avoir observé les résultats.

  1. Avant de tracer un graphique, créer plan contenant au moins cinq questions. Pour chacune, préciser l'unité, les variables, la préparation, la production attendue et la décision que cette question pourrait déclencher.
  2. Importer les données depuis R et construire un audit de la structure, des types, des valeurs manquantes, des étendues et des doublons potentiels.
  3. Construire une date à partir de `Month` et `Day`. Vérifier que le couple mois-jour peut servir de clé dans ce fichier et signaler les limites de cette clé.
  4. Produire seulement les tableaux et graphiques annoncés dans le plan : données manquantes, distribution de `Ozone`, évolution temporelle et relations avec `Temp` et `Wind`.
  5. Créer un rapport indiquant les anomalies découvertes, la décision prise, sa justification et son effet sur les analyses suivantes.
  6. Distinguer dans un court compte rendu les résultats prévus, les constats inattendus et les nouvelles hypothèses. Définir une règle d'arrêt pour cette première exploration.
]

#exercise("9", title: "Nettoyage et exploration d'un tableau désordonné")[
  Le fichier
  #link("https://stt2200.netlify.app/include/data/tp/data_messy.csv")[`data_messy.csv`]
  contient des informations synthétiques sur des personnes. Il comporte
  volontairement des doublons, formats incompatibles, valeurs manquantes et
  valeurs suspectes.

  1. Importer le fichier sans convertir automatiquement les chaînes en
     facteurs. Définir explicitement les codes de valeurs manquantes.
  2. Construire un audit indiquant dimensions, types, identifiants dupliqués,
     valeurs manquantes et modalités des variables qualitatives.
  3. Créer une table nettoyée en traitant au minimum :

     - doublons d'identifiant ;
     - courriels invalides et numéros de téléphone hétérogènes ;
     - titres dans les noms ;
     - pays et réponses oui/non ;
     - dates écrites selon plusieurs formats ;
     - salaires non finis.

  4. Repérer les salaires et nombres d'enfants atypiques par la règle de l'étendue interquartile. Les conserver avec un indicateur plutôt que les supprimer automatiquement.
  5. Réaliser une exploration univariée et bivariée : distributions, fréquences, valeurs manquantes, relation entre salaire et nombre d'enfants.
  6. Produire un rapport des transformations et expliquer pourquoi les colonnes d'adresse, de téléphone et de courriel demandent une protection particulière.
]

// Garder la simulation et la validation croisée sur la même page.
#pagebreak(weak: true)

#exercise("10", title: "Illustrer le compromis biais-variance par simulation")[
  La vraie fonction est

  $ f(x)=3+8x+2x^2, $

  et les données suivent $Y=f(X)+epsilon$, avec
  $X tilde cal(N)(0,1)$, $epsilon tilde cal(N)(0,5)$, indépendants.

  1. Simuler un échantillon d'entraînement de taille $50$ et représenter le
     nuage avec la vraie courbe.
  2. Ajuster des polynômes de degrés $1$, $2$ et $10$. Comparer leurs erreurs
     d'entraînement.
  3. Répéter l'expérience $500$ fois. Sur une grille fixe de $[-1.5,1.5]$,
     stocker les prédictions de chaque procédure.
  4. Estimer, pour chaque degré, le biais au carré intégré, la variance intégrée
     et l'erreur prédictive attendue $5+"biais"^2+"variance"$.
  5. Tracer quelques courbes ajustées pour visualiser la stabilité.
  6. Expliquer pourquoi le domaine choisi pour mesurer le biais et la variance
     doit être annoncé.
]

// Le modèle visé par le renvoi du livre est celui de l'exercice 4.26.
#exercise("11", title: "Programmer une validation croisée sans fonction spécialisée")[
  On ne connaît plus la forme de $f$. Il faut sélectionner le degré polynomial
  uniquement à partir des données.

  1. Simuler $150$ observations avec le modèle de l'exercice précédent.
  2. Construire aléatoirement cinq plis de tailles aussi égales que possible.
  3. Écrire une fonction `cv_polynomial` qui, pour un degré donné, ajuste cinq
     modèles et renvoie les erreurs quadratiques de validation.
  4. Comparer les degrés $1$ à $10$ par l'erreur moyenne et son erreur-type.
  5. Appliquer la règle du minimum, puis la règle d'une erreur-type.
  6. Expliquer comment imbriquer ce choix si une estimation finale non biaisée
     est requise.
]
