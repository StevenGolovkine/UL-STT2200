############################################################
## Packages
############################################################
install.packages(c("FactoMineR", "factoextra", "dplyr"))
library(FactoMineR)
library(factoextra)
library(dplyr)

############################################################
## EXERCICE 1 : Coquillages
############################################################

## 1. Importer les données
## Remplace "coquillages.csv" par le vrai fichier
shells <- read.csv("coquillages.csv")

## Supposons : 7 variables quantitatives + colonnes Genre, Age, etc.
## Adaptez les noms des colonnes selon votre fichier.
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
fviz_pca_ind(res.pca, habillage = shells$Genre)      # individus, colorés par genre (exemple)

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
