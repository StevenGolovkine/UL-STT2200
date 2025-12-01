############################################################
# TP : Généralités - Code R
############################################################

# Install packages once if needed:
# install.packages(c("tidyverse", "lubridate"))

library(tidyverse)
library(lubridate)

############################################################
# Exercice 1 : Nettoyage et exploration de données
############################################################

# 1. Télécharger le jeu de données
# Remplacer "data.csv" par le vrai fichier téléchargé
raw <- read.csv("./data.csv", na.strings = c("", "NA", "N/A", "?", "NULL"))

# Copie de travail
df <- raw

#-----------------------------------------------------------
# 2. Nettoyage du jeu de données
#-----------------------------------------------------------

# Le code ci-dessous présente des exemples de choses à faire pour nettoyer les
# données. Toutes ne sont pas nécessaires pour ce jeu de données spécifique,
# mais peuvent vous servir pour d'autres jeux de données.

# 2.1 Encodage des valeurs manquantes
# Exemple : certaines colonnes utilisent "-999" pour signifier manquant
df <- df |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(.x %in% c(-999, -99), NA, .x)
    )
  )

# 2.2 Gestion des valeurs extrêmes / aberrantes (exemple sur une variable
# "age" et "income")
# Ici, on fixe des bornes raisonnables puis on met hors bornes à NA
df <- df |>
  mutate(
    age   = ifelse(age < 0 | age > 120, NA, age),
    income = ifelse(income < 0 | income > 1e6, NA, income)
  )

# 2.3 Gestion des doublons (sur toutes les colonnes)
df <- df |>
  distinct()

# 2.4 Harmonisation des dates
df <- df |>
  mutate(
    date  = dmy(date),   # ou ymd, mdy selon le format
  )

# 2.5 Suppression des tirets, points, espaces, etc. dans les numéros de téléphone
# Exemple : colonne "telephone"
df <- df |>
  mutate(
    telephone = telephone |>
      str_replace_all("[^0-9]", "")  # garde seulement les chiffres
  )

# 2.6 Gestion des titres (Mr., Ms., Dr., …) dans les noms
# Exemple : colonne "nom_complet"
df <- df |>
  mutate(
    titre = str_extract(name, "^(Mr\\.?|Ms\\.?|Mrs\\.?|Dr\\.?)"),
    nom_complet_sans_titre = str_trim(
      str_remove(name, "^(Mr\\.?|Ms\\.?|Mrs\\.?|Dr\\.?)\\s*")
    )
  ) |> 
  mutate(
    titre = str_replace_all(titre, "\\.", "")
  )

# 2.7 Harmonisation des pays
# Exemple : colonne "country"
df <- df |>
  mutate(
    country = str_to_lower(country),
    country = case_when(
      country %in% c("france", "fr", "fra") ~ "France",
      country %in% c("canada", "ca", "can") ~ "Canada",
      country %in% c("usa", "us", "etats-unis", "états-unis", "united states") ~ "United States",
      TRUE ~ str_to_title(country)
    )
  )

# 2.8 Conversion des Oui/Non en TRUE/FALSE
# Exemple : colonne "have_car"
df <- df |>
  mutate(
    have_car = case_when(
      have_car %in% c("Oui", "oui", "OUI", "Yes", "yes") ~ TRUE,
      have_car %in% c("Non", "non", "NON", "No", "no")   ~ FALSE,
      TRUE ~ NA
    )
  )


#-----------------------------------------------------------
# 3. Exploration unidimensionnelle / bidimensionnelle
#-----------------------------------------------------------

# Le code ci-dessous présente des exemples de choses à faire pour une
# exploration unidimensionelle et bidimensionnelle. Tout n'est pas nécessaire
# pour ce jeu de données et dépend du contexte.

# 3.1 Statistiques descriptives pour les variables quantitatives
df_num <- df |> select(where(is.numeric))

summary(df_num)

# Histogrammes pour quelques variables numériques
df_num |> 
  pivot_longer(everything(), names_to = "variable", values_to = "valeur") |>
  ggplot(aes(x = valeur)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

# 3.2 Corrélations
cor(df_num, use = "pairwise.complete.obs")

# 3.3 Tableaux de fréquences pour variables qualitatives
df_cat <- df |> select(where(~ is.character(.x) | is.factor(.x)))

# Exemple de fréquence pour une variable catégorielle
table(df$country)
prop.table(table(df$country))

# Exemple de tableau croisé
table(df$country, df$have_car)

# 3.4 Graphiques bivariés (exemples)
# nuage de points entre deux variables quantitatives
ggplot(df, aes(x = nb_child, y = salary)) +
  geom_point(alpha = 0.6) +
  theme_minimal()

# boîte à moustaches d’une variable quantitative selon une variable qualitative
ggplot(df, aes(x = country, y = salary)) +
  geom_boxplot() +
  theme_minimal() +
  coord_flip()


############################################################
# Exercice 2 : Compromis biais-variance
############################################################

set.seed(123)

# Vraie fonction
f_true <- function(x) 3 + 8 * x + 2 * x^2

sigma2 <- 10
sigma  <- sqrt(sigma2)

# Fonction pour simuler un jeu de données
simulate_data <- function(n) {
  X <- rnorm(n, mean = 0, sd = 1)
  eps <- rnorm(n, mean = 0, sd = sigma)
  Y <- f_true(X) + eps
  tibble(X = X, Y = Y)
}

# Fonction pour ajuster un modèle polynomial de degré 'deg'
fit_poly_model <- function(train_data, deg = 1) {
  lm(Y ~ poly(X, deg, raw = TRUE), data = train_data)
}

# Fonction pour calculer MSE, biais et variance sur un jeu de validation
eval_model <- function(model, val_data) {
  # prédictions
  y_hat <- predict(model, newdata = val_data)
  
  # vraie valeur sans bruit
  f_x  <- f_true(val_data$X)
  
  # MSE sur la validation (entre Y observé et Y chapeau)
  mse  <- mean((val_data$Y - y_hat)^2)
  
  # "Biais" empirique : différence moyenne entre f(x) et y_hat
  bias <- mean(f_x - y_hat)
  bias2 <- mean((f_x - y_hat)^2)   # biais^2 moyen
  
  # Variance des prédictions (variabilité de y_hat autour de sa moyenne)
  var_hat <- var(y_hat)
  
  list(
    mse   = mse,
    bias  = bias,
    bias2 = bias2,
    var_pred = var_hat
  )
}

# 1. Simuler un ensemble de données (X,Y)
n_train <- 200
train_data <- simulate_data(n_train)

plot(train_data$X, train_data$Y)

# 2. Ajuster un modèle linéaire Y = beta0 + beta1 X
mod_deg1 <- fit_poly_model(train_data, deg = 1)

# 3. Calculer \hat{Y} sur le jeu d'entraînement
train_data <- train_data |>
  mutate(
    Y_hat_deg1 = predict(mod_deg1, newdata = train_data)
  )

# 4. Erreur quadratique moyenne sur le jeu d'entraînement
mse_train_deg1 <- mean((train_data$Y - train_data$Y_hat_deg1)^2)
mse_train_deg1

# 5. Jeu de validation + MSE, biais, variance
n_val <- 1000
val_data <- simulate_data(n_val)

eval_deg1 <- eval_model(mod_deg1, val_data)
eval_deg1

# 6. Même chose pour un polynôme d’ordre 2
mod_deg2 <- fit_poly_model(train_data, deg = 2)
train_data$Y_hat_deg2 <- predict(mod_deg2, newdata = train_data)
mse_train_deg2 <- mean((train_data$Y - train_data$Y_hat_deg2)^2)
mse_train_deg2

eval_deg2 <- eval_model(mod_deg2, val_data)
eval_deg2

# 7. Même chose pour un polynôme d’ordre 10
mod_deg10 <- fit_poly_model(train_data, deg = 10)
train_data$Y_hat_deg10 <- predict(mod_deg10, newdata = train_data)
mse_train_deg10 <- mean((train_data$Y - train_data$Y_hat_deg10)^2)
mse_train_deg10

eval_deg10 <- eval_model(mod_deg10, val_data)
eval_deg10

# Vous pouvez comparer eval_deg1, eval_deg2, eval_deg10 pour discuter
# du compromis biais-variance et de la complexité du modèle.
plot(train_data$X, train_data$Y)
points(train_data$X, train_data$Y_hat_deg1, col = 'red')
points(train_data$X, train_data$Y_hat_deg2, col = 'blue')
points(train_data$X, train_data$Y_hat_deg10, col = 'green')


############################################################
# Exercice 3 : Validation croisée (K = 3)
############################################################

# 1. Simuler un jeu de données (même modèle que l'exercice 2)
set.seed(456)
n <- 300
data_cv <- simulate_data(n)

# Fonction K-fold CV "maison" pour un modèle polynomial
kfold_cv <- function(data, K = 3, deg = 1) {
  n <- nrow(data)
  
  # Mélange aléatoire des indices
  indices <- sample(1:n)
  folds <- cut(seq_along(indices), breaks = K, labels = FALSE)
  
  mse_k <- numeric(K)
  
  for (k in 1:K) {
    # Indices de validation pour ce fold
    val_idx <- indices[folds == k]
    train_idx <- setdiff(indices, val_idx)
    
    train_data <- data[train_idx, ]
    val_data   <- data[val_idx, ]
    
    # Ajustement du modèle
    model <- lm(Y ~ poly(X, deg, raw = TRUE), data = train_data)
    
    # Prédiction sur le fold de validation
    y_hat <- predict(model, newdata = val_data)
    
    # MSE sur ce fold
    mse_k[k] <- mean((val_data$Y - y_hat)^2)
  }
  
  # MSE moyen de validation
  mean(mse_k)
}

# 2. CV pour la régression linéaire simple (deg = 1)
cv_mse_deg1 <- kfold_cv(data_cv, K = 3, deg = 1)
cv_mse_deg1

# 3. CV pour le modèle polynômial d’ordre 2
cv_mse_deg2 <- kfold_cv(data_cv, K = 3, deg = 2)
cv_mse_deg2

# 4. CV pour le modèle polynômial d’ordre 10
cv_mse_deg10 <- kfold_cv(data_cv, K = 3, deg = 10)
cv_mse_deg10

# 5. Comparez cv_mse_deg1, cv_mse_deg2, cv_mse_deg10
# pour discuter de l'utilisation de la validation croisée
# dans le choix du degré de polynôme (complexité du modèle).
