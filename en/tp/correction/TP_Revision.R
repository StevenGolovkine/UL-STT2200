############################################################
## Exercice 1 : Estimer pi par Monte-Carlo
############################################################

set.seed(42)  # pour la reproductibilité

# 1–4. Estimation de pi avec n points
mc_pi <- function(n) {
  x <- runif(n, -1, 1)
  y <- runif(n, -1, 1)
  inside <- x^2 + y^2 <= 1
  pi_hat <- 4 * mean(inside)
  return(pi_hat)
}

# Exemple : estimation pour n = 10 000
n_example <- 10000
pi_hat_example <- mc_pi(n_example)
pi_hat_example

# 5. Tracer l’erreur |pi - pi_hat(n)| en fonction de n
#    On utilise une trajectoire cumulative pour n = 1,...,Nmax

Nmax <- 10000
x <- runif(Nmax, -1, 1)
y <- runif(Nmax, -1, 1)
inside <- x^2 + y^2 <= 1

pi_hat_seq <- 4 * cumsum(inside) / (1:Nmax)
err_seq <- abs(pi - pi_hat_seq)

plot(1:Nmax, err_seq,
     type = "l",
     xlab = "n",
     ylab = "Erreur |pi - pi_hat(n)|",
     main = "Erreur de l'estimateur Monte-Carlo de pi",
     log = "x")  # échelle log sur l’axe des x (optionnel)

abline(h = 0, lty = 2, col = "grey")


############################################################
## Exercice 2 : Estimation de l’intégrale I = ∫_0^1 sqrt(1 - x^2) dx
############################################################

# Fonction f(x) = sqrt(1 - x^2)
f <- function(x) sqrt(1 - x^2)

# 1. Calcul exact de I avec la primitive (I = pi/4 pour cette intégrale)
I_exact <- pi / 4
I_exact

# (Option : vérification numérique)
integrate(f, lower = 0, upper = 1)

# 2. Sommes de Riemann : I_hat(n) = (1/n) * sum_{i=1}^n f(i/n)
riemann_estimator <- function(n) {
  i <- 1:n
  mean(f(i / n))   # (1/n) * sum f(i/n)
}

# Exemple pour quelques n
n_values <- c(10, 50, 100, 500, 1000, 5000, 10000)

I_riemann <- sapply(n_values, riemann_estimator)
err_riemann <- abs(I_riemann - I_exact)

data.frame(
  n = n_values,
  I_riemann = I_riemann,
  erreur = err_riemann
)

# 3. Estimateur de Monte-Carlo : I_tilde(n) = (1/n) * sum f(U_i), U_i ~ U(0,1)

set.seed(42)  # pour la reproductibilité

mc_integral <- function(n) {
  u <- runif(n, 0, 1)
  mean(f(u))
}

I_mc <- sapply(n_values, mc_integral)
err_mc <- abs(I_mc - I_exact)

data.frame(
  n = n_values,
  I_mc = I_mc,
  erreur = err_mc
)

# 4. Comparaison des erreurs en fonction de n

plot(n_values, err_riemann,
     type = "b",
     log = "x",
     xlab = "n",
     ylab = "Erreur absolue",
     pch = 16,
     main = "Erreur : sommes de Riemann vs Monte-Carlo")
lines(n_values, err_mc, type = "b", pch = 17)
legend("topright",
       legend = c("Sommes de Riemann", "Monte-Carlo"),
       pch = c(16, 17),
       bty = "n")


############################################################
## Exercice 3 : Loi des gaz parfaits
############################################################

# Données (T en K, P en kPa)
temperature <- c(
  406, 296, 272, 449, 483, 439, 460, 276, 321, 462, 408, 322, 285,
  411, 491, 359, 453, 486, 413, 350, 263, 456, 390, 462, 389, 494,
  303, 496, 336, 460
)

pression <- c(
  1365, 982, 898, 1486, 1596, 1481, 1506, 906, 1085, 1542, 1367,
  1072, 955, 1379, 1633, 1186, 1499, 1606, 1378, 1156, 867, 1514,
  1306, 1525, 1287, 1665, 1020, 1635, 1118, 1529
)

length(temperature); length(pression)  # vérification

# 1. Tracer P en fonction de T
plot(temperature, pression,
     xlab = "Température (K)",
     ylab = "Pression (kPa)",
     main = "Pression en fonction de la température")
grid()
# (Option : ajouter la droite de régression pour visualiser la linéarité)
abline(lm(pression ~ temperature), col = "red", lwd = 2)

# 2. Construire la matrice X = (1 | temperature)
X <- cbind(1, temperature)  # première colonne de 1, deuxième = température
Y <- pression

# 3. Calculer beta = (X^T X)^(-1) X^T Y
beta <- solve(t(X) %*% X) %*% (t(X) %*% Y)
beta

beta0 <- beta[1]  # intercept (en kPa)
beta1 <- beta[2]  # pente (kPa par Kelvin)

beta0
beta1

# Vérification avec lm (devrait donner la même chose)
coef(lm(pression ~ temperature))

# 4. Interprétations physiques (commentaires dans le rapport, pas dans le code)
# - beta0 : pression "théorique" à T = 0 K (en kPa).
# - beta1 : variation de pression (kPa) par unité de température (1 K).

# 5. Dans le modèle de gaz parfait P V = n R T,
#    P = (n R / V) * T, donc la droite passe par l’origine => beta0 attendu = 0.
#    On peut regarder la valeur estimée de beta0 (déjà dans beta0).

beta0  # à comparer qualitativement avec 0

# 6. Estimation de n (quantité de matière) pour V = 10 dm^3
#    Attention aux unités :
#    - P est en kPa, donc on le convertit en Pa pour utiliser R = 8.314 J/(mol*K).
#    - 10 dm^3 = 0.01 m^3.

R_const <- 8.314          # J/mol/K
V_m3 <- 10e-3             # 10 dm^3 = 0.01 m^3
slope_kPa_per_K <- as.numeric(beta1)
slope_Pa_per_K <- slope_kPa_per_K * 1000  # conversion kPa -> Pa

# D'après P = (n R / V) * T, la pente = n R / V  =>  n = slope * V / R
n_hat <- slope_Pa_per_K * V_m3 / R_const
n_hat  # en moles
