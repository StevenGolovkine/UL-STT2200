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
#let document-title = "Corrigé - Rappels"

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

= Réponses

#let exercise-counter = counter("exercise")

#let exercise(title: none, body) = {
  exercise-counter.step()
  heading(level: 2)[
    Exercice #context exercise-counter.display()
    #if title != none [ — #title]
  ]
  body
}

#let answer(body) = block(
  width: 100%,
  inset: 10pt,
  radius: 4pt,
  fill: luma(245),
  stroke: 0.6pt + luma(180),
  body,
)

// Conserver le même ordre et les mêmes titres que dans exercises-rappels.typ.
#exercise(title: "Combinaisons linéaires et matrice de covariance")[
  #answer[
    1. La matrice est symétrique, son premier mineur principal vaut $4>0$ et son
     déterminant vaut $4 times 9-1.5^2=33.75>0$. Le critère de Sylvester montre
     qu'elle est définie positive.

  2. Les écarts-types sont $2$ et $3$, d'où
     $rho_(X Y)=frac(1.5, 2 times 3, style: "horizontal")=0.25$.

  3. Avec $a=(2,-1)^top$, $Z=a^top (X,Y)^top$. Ainsi,

     $ E(Z)=a^top mu=2 times 2-(-1)=5, $

     $ "Var"(Z)=a^top Sigma a
       =4 "Var"(X)+"Var"(Y)-4 "Cov"(X,Y)=19. $

  4. Pour $b=(1,1)^top$,

     $ E(W)=b^top mu=1, quad "Var"(W)=b^top Sigma b=16, $

     et $"Cov"(Z,W)=a^top Sigma b=0.5$.

  5. Une covariance nulle ne suffit généralement pas à établir l'indépendance.
     Elle l'implique toutefois pour un vecteur conjointement gaussien. Ici, la
     covariance calculée n'est d'ailleurs pas nulle.
  ]
]


#exercise(title: "Combinaisons linéaires et matrice de covariance")[
  #answer[
    1. Le sexe de la cadette est indépendant de celui de l'aînée. La probabilité demandée vaut $frac(1, 2, style: "horizontal")$.

    2. Avant conditionnement, les quatre couples ordonnés FF, FG, GF et GG sont équiprobables. L'information « au moins un garçon » élimine seulement FF. Il reste trois possibilités, dont une seule GG : la probabilité vaut $frac(1, 3, style: "horizontal")$.

    3. Le choix aléatoire désigne un enfant précis. Conditionnellement au fait que celui-ci est un garçon, le sexe de l'autre reste indépendant : la réponse est $frac(1, 2, style: "horizontal")$. Dans la question 2, aucune position n'est désignée et les familles mixtes possèdent deux ordres possibles.

    4. La fille rencontrée est identifiée. Son jour de naissance ne renseigne pas sur le sexe de l'autre enfant, dont la probabilité d'être une fille reste $frac(1, 2, style: "horizontal")$.

    5. Chaque enfant possède $14$ états sexe-jour. Parmi les $14^2=196$ couples, $196-13^2=27$ contiennent au moins une fille née vendredi. Parmi les $7^2=49$ couples de deux filles, $49-6^2=13$ satisfont cette condition. La probabilité vaut donc $frac(13, 27, style: "horizontal")$, et non $frac(1, 2, style: "horizontal")$.

    6. Une probabilité conditionnelle dépend du mécanisme qui produit l'information. Les formulations « un enfant identifié », « au moins un » et « une réponse choisie par le parent » ne définissent pas le même événement. Sans protocole d'observation, le problème peut être ambigu.
  ]
]

#exercise(title: "Combinaisons linéaires et matrice de covariance")[
  #answer[
  1. Sur $10 000$ personnes, on attend $200$ personnes atteintes : $180$
     positives et $20$ négatives. Parmi les $9800$ personnes non atteintes,
     $490$ sont faussement positives et $9310$ négatives.

  2. La probabilité marginale d'un positif vaut

     $ P(+)=0.02 times 0.90+0.98 times 0.05=0.067. $

     Par Bayes,

     $ P(D|+)=0.018/0.067 approx 0.2687. $

  3. On obtient

     $ P(overline(D)|-)
       =(0.98 times 0.95)/(0.98 times 0.95+0.02 times 0.10)
       =0.931/0.933 approx 0.9979. $

  4. La faible prévalence produit beaucoup plus de personnes non atteintes. Même
     un faible taux de faux positifs appliqué à ce grand groupe peut dépasser le
     nombre de vrais positifs.

  5. Sous l'indépendance conditionnelle,

     $ P(D|+,+)
       =(0.02 times 0.90^2)/(0.02 times 0.90^2+0.98 times 0.05^2)
       approx 0.8686. $

  6. Une cause commune, comme une interférence propre à l'échantillon, peut
     provoquer deux erreurs semblables. L'indépendance surestimerait alors
     l'information supplémentaire du second test.
  ]
]

#exercise(title: "Estimation par Monte-Carlo")[
  #answer[
    La fonction suivante vectorise les $n$ tests d'appartenance.

  ```r
  estimate_pi <- function(n) {
    x <- runif(n, -1, 1)
    y <- runif(n, -1, 1)
    4 * mean(x^2 + y^2 <= 1)
  }

  set.seed(2200)
  n <- 100000
  pi_hat <- estimate_pi(n)
  pi_hat
  ```

  Avec cette graine, $hat(pi) approx 3.14284$. Pour éviter un graphique trop
  lourd, on peut n'afficher que les $5000$ premiers points.

  ```r
  keep <- seq_len(5000)
  plot(
    x[keep], y[keep], asp = 1, pch = 16, cex = 0.35,
    col = ifelse(inside[keep], "steelblue", "tomato"),
    xlab = "x", ylab = "y"
  )
  theta <- seq(0, 2 * pi, length.out = 500)
  lines(cos(theta), sin(theta), lwd = 2)
  ```

  La moyenne cumulée fournit toutes les estimations sans refaire les tirages.

  ```r
  pi_path <- 4 * cumsum(inside) / seq_len(n)
  grid <- unique(round(10^seq(1, 5, length.out = 100)))

  plot(
    grid, abs(pi_path[grid] - pi), log = "xy",
    type = "l", xlab = "Nombre de points",
    ylab = "Erreur absolue"
  )
  ```

  L'erreur n'est pas monotone : ajouter des points peut momentanément éloigner
  l'estimation de $pi$. Son ordre typique diminue cependant comme $n^(-frac(1, 2, style: "horizontal"))$.
  En effet, si $p=frac(pi, 4, style: "horizontal")$,

  $ "Var"(hat(pi))=16 p(1-p)/n. $

  ```r
  set.seed(2200)
  estimates <- replicate(1000, estimate_pi(2000))
  c(mean = mean(estimates), sd = sd(estimates))

  p <- pi / 4
  theoretical_sd <- 4 * sqrt(p * (1 - p) / 2000)
  theoretical_sd
  ```

  On obtient une moyenne d'environ $3.14175$, un écart-type empirique de
  $0.03712$ et une valeur théorique de $0.03672$.

  Pour le grand échantillon initial, un intervalle approché est

  ```r
  p_hat <- mean(inside)
  se <- 4 * sqrt(p_hat * (1 - p_hat) / n)
  pi_hat + c(-1, 1) * qnorm(0.975) * se
  ```

  Il vaut environ $[3.1327,3.1530]$. Avec un petit $n$, l'approximation normale
  et le remplacement de $p$ par $hat(p)$ peuvent être médiocres ; un intervalle
  binomial pour $p$, ensuite multiplié par $4$, est préférable.
  ]
]

#exercise(title: "Loi des gaz parfaits")[
  #answer[
    Les deux vecteurs contiennent $30$ valeurs sans donnée manquante. Le nuage est
  très proche d'une droite croissante.

  ```r
  plot(
    temperature, pressure, pch = 16,
    xlab = "Température (K)", ylab = "Pression (kPa)"
  )

  X <- cbind(intercept = 1, temperature = temperature)
  Y <- pressure
  beta <- solve(crossprod(X), crossprod(X, Y))
  beta
  ```

  Le calcul matriciel donne
  $hat(beta)_0 approx 4.1676$ kPa et
  $hat(beta)_1 approx 3.3150$ kPa/K.

  ```r
  fit <- lm(pressure ~ temperature)
  coef(fit)

  fitted_values <- drop(X %*% beta)
  residuals <- Y - fitted_values
  r_squared <- 1 - sum(residuals^2) / sum((Y - mean(Y))^2)
  r_squared
  ```

  Les coefficients coïncident avec ceux de `lm` à l'arrondi près et
  $R^2 approx 0.9980$. L'ordonnée à l'origine est la pression extrapolée à
  $T=0$ K ; le modèle idéal prévoit zéro. La valeur estimée est faible au regard
  des pressions mesurées et son incertitude est importante, car zéro kelvin est
  très loin de la plage observée.

  La pente théorique vérifie, lorsque $P$ est exprimée en pascals,
  $beta_1=frac(n R, V, style: "horizontal")$. Avec $V=10 " dm"^3=0.01 " m"^3$ :

  ```r
  R <- 8.314
  volume_m3 <- 10e-3
  slope_pa <- beta[2] * 1000
  n_moles <- slope_pa * volume_m3 / R
  n_moles
  ```

  On estime $n approx 3.99$ moles.

  ```r
  plot(
    temperature, residuals, pch = 16,
    xlab = "Température (K)", ylab = "Résidu (kPa)"
  )
  abline(h = 0, lty = 2)
  ```

  Un excellent alignement confirme la compatibilité des données avec la loi sur
  cette plage. Il ne vérifie pas indépendamment l'étalonnage des instruments,
  la constance réelle du volume et de la quantité de matière, ni l'absence de
  variables confondantes. L'extrapolation jusqu'à $0$ K reste particulièrement
  fragile.
  ]
]

#exercise(title: "Loi d'Ohm, covariance et ajustement linéaire")[
  #answer[
    1. Les moyennes sont
     $overline(A)=0.9857$ A et $overline(V)=20.6429$ V.

  2. Avec le diviseur $n=7$,

     $ "Var"(A) approx 0.23837, quad
       "Var"(V) approx 106.5396, quad
       "Cov"(A,V) approx 5.03918. $

     L'emploi du diviseur $n-1$ multiplierait les trois quantités par $frac(7, 6, style: "horizontal")$ ;
     il ne changerait donc ni la corrélation ni la pente.

  3. La corrélation vaut environ $0.99996$. Le nuage est presque parfaitement
     aligné et croissant. Une corrélation, même très élevée, ne suffit toutefois
     pas à établir seule une causalité ou la validité d'une loi physique.

  4. On obtient

     $ R=5.03918/0.23837 approx 21.1404 " ohms", $

     puis $beta_0=overline(V)-R overline(A) approx -0.1955$ V. Le modèle ajusté est donc
     $hat(V)=-0.1955+21.1404 A$.

  5. Sans constante, la pente minimise $sum_i (V_i-R A_i)^2$ et vaut

     $ hat(R)_0=(sum_i A_i V_i)/(sum_i A_i^2) approx 20.9811 " ohms". $

     Les deux valeurs sont proches, car l'ordonnée à l'origine libre est déjà
     voisine de zéro.

  6. Les rapports observés sont approximativement $20.00$, $20.80$, $20.78$,
     $21.10$, $20.92$, $21.08$ et $21.00$. Les résidus du modèle avec constante
     ont une somme nulle et une somme des carrés d'environ $0.0643$. Les écarts
     proviennent notamment du bruit de mesure, des arrondis et des limites du
     modèle idéal ; diviser observation par observation n'agrège pas le bruit de
     la même façon que les moindres carrés.
  ]
]

#exercise(title: "Loi des grands nombres et théorème central limite")[
  #answer[
    Une matrice stockant une répétition par ligne permet de vectoriser les
  moyennes.

  ```r
  simulate_exponential <- function(n, B = 5000, rate = 2) {
    samples <- matrix(rexp(B * n, rate), nrow = B, ncol = n)
    means <- rowMeans(samples)
    sds <- apply(samples, 1, sd)

    list(
      means = means,
      sds = sds,
      z = sqrt(n) * (means - 0.5) / 0.5
    )
  }

  set.seed(2200)
  sizes <- c(5, 30, 100)
  simulations <- lapply(sizes, simulate_exponential)
  names(simulations) <- sizes
  ```

  ```r
  par(mfrow = c(1, 3))
  for (n in sizes) {
    z <- simulations[[as.character(n)]]$z
    hist(
      z, probability = TRUE, breaks = 40,
      main = paste("n =", n), xlab = "z"
    )
    curve(dnorm(x), add = TRUE, col = 2, lwd = 2)
  }
  par(mfrow = c(1, 1))
  ```

  Pour $n=5$, l'histogramme conserve une asymétrie à droite. Celle-ci diminue
  pour $n=30$ et $n=100$, conformément au théorème central limite.

  ```r
  diagnostics <- t(vapply(simulations, function(out) {
    c(
      mean = mean(out$z),
      variance = var(out$z),
      q025 = unname(quantile(out$z, 0.025)),
      q975 = unname(quantile(out$z, 0.975))
    )
  }, numeric(4)))
  diagnostics
  ```

  Les cibles sont $0$, $1$, $-1.96$ et $1.96$. Elles sont approchées de mieux en
  mieux lorsque $n$ augmente ; des écarts de simulation d'ordre $B^(-frac(1, 2, style: "horizontal"))$
  subsistent.

  ```r
  known_sd_coverage <- vapply(seq_along(sizes), function(i) {
    n <- sizes[i]
    means <- simulations[[i]]$means
    half_width <- 1.96 * 0.5 / sqrt(n)
    mean(means - half_width <= 0.5 &
         0.5 <= means + half_width)
  }, numeric(1))

  estimated_sd_coverage <- vapply(seq_along(sizes), function(i) {
    n <- sizes[i]
    out <- simulations[[i]]
    half_width <- 1.96 * out$sds / sqrt(n)
    mean(out$means - half_width <= 0.5 &
         0.5 <= out$means + half_width)
  }, numeric(1))

  data.frame(
    n = sizes,
    known_sd_coverage,
    estimated_sd_coverage
  )
  ```

  Les couvertures se rapprochent de $95%$ avec $n$. Pour un petit échantillon,
  la normalité approximative est moins précise et l'estimation de $sigma$ ajoute
  de la variabilité. Un intervalle fondé sur la loi exacte de la somme des
  exponentielles ou une méthode adaptée à l'asymétrie peut alors être préférable.
  ]
]