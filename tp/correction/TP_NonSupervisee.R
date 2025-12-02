############################################################
## Packages
############################################################

library(tidyverse)
library(ipred)
library(cluster)

############################################################
## EXERCICE 1: Arrestation aux USA
############################################################

# 1. Importer les données
df <- read.csv('./USArrests.csv')
df <- df |> column_to_rownames(var = 'X')


# 2. CAH, méthode des plus proches voisins et distance euclidienne
dist <- dist(df)

model <- hclust(dist, method = 'average')  # On remplace single par average, ça a plus de sens.
plot(model)


# 3. On coupe pour avoir 3 groupes.
clusters <- cutree(model, k = 3)

df |> filter(clusters == 1)
df |> filter(clusters == 2)
df |> filter(clusters == 3)

# 4. On standardise
df_scale <- scale(df)
dist <- dist(df_scale)
model <- hclust(dist, method = 'average')  # On remplace single par average, ça a plus de sens.
plot(model)

clusters <- cutree(model, k = 3)

df |> filter(clusters == 1)
df |> filter(clusters == 2)
df |> filter(clusters == 3)

# 5. La standardisation n'est pas forcement utile ici.

############################################################
## EXERCICE 2 : De la musique
############################################################

# 1. Importer les données
df <- read.csv('./musique.csv', sep = ';')
df_sub <- df |> select(m1, m2, m3, m4, m5, m6, m7, m8, m9)

# 2. CAH, méthode de la moyenne et distance euclidienne
dist <- dist(df_sub)

model <- hclust(dist, method = 'average')
clusters <- cutree(model, k = 6)

# 3. CDH, on oublie...

# 4. Critère de Silhouette
si <- silhouette(clusters, dist = dist)
plot(si)
mean(si[,3])  # Le clustering est OK.

############################################################
## EXERCICE 3 : De la musique (suite)
############################################################

# 1. Importer les données
df <- read.csv('./musique.csv', sep = ';')

# 2. k-means avec K = 4
# On ne sélectionne que les variables quantitative pour faire le clustering
df_sub <- df |> select(m1, m2, m3, m4, m5, m6, m7, m8, m9)

model <- kmeans(df_sub, centers = 4)

# 3. Statistiques descriptives pour chacun des groupes

g1 <- df |> filter(model$cluster == 1)
g2 <- df |> filter(model$cluster == 2)
g3 <- df |> filter(model$cluster == 3)
g4 <- df |> filter(model$cluster == 4)

summary(g1)
summary(g2)
summary(g3)
summary(g4)

# 4. Taille des groupes
table(model$cluster)
