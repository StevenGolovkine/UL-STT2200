
library(tidyverse)

df <- read_csv('./archive/spotify_songs.csv')

df_sub <- df |> filter(
  track_artist == 'The Weeknd' | 
  track_artist == 'Bruno Mars' |
  track_artist == 'Justin Bieber' |
  track_artist == 'Lady Gaga' |
  track_artist == 'Coldplay' |
  track_artist == 'Ed Sheeran' |
  track_artist == 'Rihanna' |
  track_artist == 'Billie Eilish' |
  track_artist == 'Taylor Swift' |
  track_artist == 'Drake'
) |> 
  distinct(pick(track_name), .keep_all = TRUE) |> 
  select(
    track_name, track_artist, track_popularity,
    danceability, energy, loudness, speechiness,
    acousticness, liveness, valence, tempo 
  )
  
write_csv(df_sub, './archive/music_top10.csv')


######
######
######
library(FactoMineR)
library(factoextra)
library(tidyverse)

df <- read_csv('./archive/music_top10.csv')

knitr::kable(head(df))

ggplot(df, aes(x = danceability, y = energy, color = track_artist)) +
  geom_point() +
  theme_bw()

ggplot(df, aes(x = loudness, y = speechiness, color = track_artist)) +
  geom_point() +
  theme_bw()

# PCA not scaled
pca_not_scaled <- df |> select(
  danceability,
  energy,
  loudness,
  speechiness,
  acousticness,
  liveness,
  valence,
  tempo
) |> PCA(scale.unit = FALSE, graph = FALSE)

get_eigenvalue(pca_not_scaled)
fviz_eig(pca_not_scaled)

pca_scaled <- df |> select(
  danceability,
  energy,
  loudness,
  speechiness,
  acousticness,
  liveness,
  valence,
  tempo
) |> PCA(scale.unit = TRUE, graph = FALSE)

knitr::kable(get_eigenvalue(pca_scaled))
fviz_eig(pca_scaled)

fviz_pca_var(
  pca_scaled,
  col.var = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
  repel = TRUE # Avoid text overlapping
)


get_pca_var(pca_scaled)$cos2

fviz_contrib(pca_scaled, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_scaled, choice = "var", axes = 2, top = 10)

fviz_pca_ind(
  pca_scaled,
  geom.ind = "point", # show points only (nbut not "text")
  col.ind = df$track_artist, # color by groups
  legend.title = "Artists",
  #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  #addEllipses = TRUE, # Concentration ellipses
  #legend.title = "Groups"
) + scale_shape_manual(values = rep(19, 10))


fviz_pca_biplot(
  pca_scaled,
  repel = TRUE,
  col.var = "#2E9FDF", # Variables color
  col.ind = "#696969"  # Individuals color
)

fviz_pca_biplot(
  pca_scaled,
  repel = TRUE,
  col.ind = df$track_artist, # color by groups
  legend.title = "Artists",
  select.ind = list(cos2 = 0.75),
  col.var = "#2E9FDF", # Variables color
) + scale_shape_manual(values = rep(19, 10))
