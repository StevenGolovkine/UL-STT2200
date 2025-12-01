############################################################
## Packages
############################################################

library(FactoMineR)
library(factoextra)
library(dplyr)

############################################################
## EXERCICE 1 : Coquillages
############################################################

## 1. Importer les données
shells <- read.csv("abalone.csv")

str(shells)

############################################################
## 2. Analyse descriptive rapide
############################################################

## Statistiques descriptives sur les variables numériques
num_vars <- shells %>% 
  dplyr::select_if(is.numeric)

summary(num_vars)
cor(num_vars, use = "pairwise.complete.obs")

## Quelques graphiques de base
pairs(num_vars)                       # nuage de points pairwise
boxplot(num_vars, main = "Boxplots des mesures de coquillages")

############################################################
## 3. Analyse en composantes principales (ACP)
############################################################

## Standardisation = TRUE (classique en ACP)
res.pca <- PCA(num_vars, scale.unit = TRUE, graph = FALSE)

## Graphiques classiques
fviz_eig(res.pca, addlabels = TRUE)                  # valeurs propres
fviz_pca_var(res.pca, col.var = "contrib")           # variables
fviz_pca_ind(res.pca, col.ind = shells$gender)    # individus, colorés par gender

############################################################
## 4. Nombre d’axes à garder
############################################################

eig <- res.pca$eig
eig

## Règle de Kaiser : valeurs propres > 1
which(eig[, "eigenvalue"] > 1)

## Pourcentage cumulé de variance expliquée
eig[, "cumulative percentage of variance"]

############################################################
## 5. Contributions à la 2e et 3e dimensions
############################################################

## Contribution des variables
var_contrib <- res.pca$var$contrib   # lignes = variables, colonnes = dimensions
var_contrib_dim2 <- var_contrib[, 2]
sort(var_contrib_dim2, decreasing = TRUE)[1:2]   # 2 variables les + contributives à Dim 2

## Contribution des individus (observations)
ind_contrib <- res.pca$ind$contrib
ind_contrib_dim3 <- ind_contrib[, 3]
sort(ind_contrib_dim3, decreasing = TRUE)[1:2]   # 2 individus les + contributifs à Dim 3

############################################################
## 6. Interprétation de la première dimension
############################################################

## Vous utilisez surtout les sorties :
## - coordonnées des variables sur Dim 1
## - contributions et corr²
res.pca$var$coord[, 1]     # coordonnées des variables sur Dim 1
res.pca$var$contrib[, 1]   # contributions à Dim 1
res.pca$var$cor[, 1]^2     # cos² (qualité de représentation)

############################################################
## 7. Pourcentage de variabilité expliquée par les 3 premiers axes
############################################################

eig[1:3, "percentage of variance"]
sum(eig[1:3, "percentage of variance"])

############################################################
## 8. Pertinence de la standardisation
############################################################

## Pour discuter : comparer les ordres de grandeur des variables
apply(num_vars, 2, sd)


############################################################
## EXERCICE 2 : Écoute radio au Canada
############################################################

## 1. Importer les données
radio <- read.csv("radio.csv") |> 
  column_to_rownames(var = 'X')
str(radio)

############################################################
## 2. Analyse factorielle des correspondances (AFC)
############################################################

res.ca <- CA(radio, graph = FALSE)

## Graphiques classiques
fviz_eig(res.ca, addlabels = TRUE)                  # valeurs propres
fviz_ca_row(res.ca)                                 # lignes
fviz_ca_col(res.ca)                                # colonnes

############################################################
## 3. Premier axe factoriel
############################################################

# Le premier axe discrimine entre les ados et les adultes, ainsi que les types
# de radio / musique. Les adultes écoutant des émissions parlées, alors que les
# ados sont du côtès de "Dance".

############################################################
## 4. Qualité de représentation
############################################################

res.ca$col$contrib

# Les ados sont très bien représentés sur la première composante (et très peu
# sur le 2e composante). Les adultes sont un peu représentés sur la 1ere
# composante, mais mieux representés sur la 2e.


############################################################
## EXERCICE 3 : Café
##############################################################

# 1. Importer les données
cafe <- read.csv("coffee_sales.csv") 
str(cafe)

# 2. Tableau de fréquences relatives "Types de café" x "Période de la journée"
table(cafe$coffee_name, cafe$Time_of_Day)
freq_coffee <- prop.table(table(cafe$coffee_name, cafe$Time_of_Day))

############################################################
## 3. Analyse factorielle des correspondances (AFC)
############################################################

res.ca <- CA(freq_coffee, graph = FALSE)

## Graphiques classiques
fviz_eig(res.ca, addlabels = TRUE)                  # valeurs propres
fviz_ca_row(res.ca)                                 # lignes
fviz_ca_col(res.ca)                                # colonnes

############################################################
## 4. Axes factoriels
############################################################

# Les axes discrimine les types de cafés avec les périodes de la journée.
# En aprés-midi, des Americano et Espresso sont surtout consommés. En matinée,
# des boissons avec du lait et en soirée, plutôt des boissons sucrées avec du
# chocolat.

############################################################
## 5. Indépendance
############################################################

chisq.test(table(cafe$coffee_name, cafe$Time_of_Day))

# Les deux variables ne sont pas indépendentes.

############################################################
## EXERCICE 4 : Post-partum
##############################################################

# 1. Importer les données
postpartum <- read.csv("postpartum_data.csv") |> 
  select(-Timestamp)
str(postpartum)

# 2. Tableau disjonctif complet
tab_disj <- tab.disjonctif(postpartum)

# 3. Tableau de Burt
burt <- t(tab_disj) %*% tab_disj

############################################################
## 4. Analyse des correspondances multiples (ACM)
############################################################

res.mca <- MCA(postpartum, graph = FALSE)

fviz_eig(res.mca, addlabels = TRUE)                  # valeurs propres
fviz_mca_var(res.mca, col.var = "contrib")           # variables
fviz_mca_ind(res.mca)                                # individus

############################################################
## 5. Axes factoriels
############################################################

# Les deux premiers axes factoriels sont constuits à l'aide des modalités: 
# "Irritable towards baby", "Feeling of guilt" and "Problem concentrating". 
# Donc, ces axes concernes surtout les mères ayant des symptômes post-partum.
# Les autres modalités sont surtout concentrées autour du centre d'inertie du
# nuage de points.

############################################################
## 6. Associations
############################################################

sum(res.mca$ind$coord[,1] > 1)
sum(res.mca$ind$coord[,2] > 1)

# On remarque que les deux axes sont liés à un petit nombre de personnes.
# Il faut donc faire attention lors de l'analyse des résultats. Des personnes
# ayant des symdromes de post-partum peuvent aussi se trouver proche du 
# centre d'inertie.