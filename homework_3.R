# Homework 3
options(scipen = 5, digits = 3)
library(tidyverse)
library(lme4)
stroop_d <- read.csv("stroop_standing_data.csv")

# A within subjects 3 (data type: practice vs congruent vs incongruent) x2 (position: standing vs sitting) Anova can be conducted to analyze whether there is an effect
# of the manipulation of sitting vs standing.

# let's examine the data
summary(stroop_d)
glimpse(stroop_d)

# We need to clean up the data so they are good for the analysis
# it feels like something is missing here in the information that we were given because I do not know what the X is for
# I will drop X and "correct" as it does not denote anything of interest really
# I can also drop the baseline congruency and practice phase, as these will not be analyzed
d = subset(stroop_d, select = -c(X, correct))
stroop_filtered <- filter(d, congruency != "baseline", phase != "practice")


# Now that is done, we might want to drop the trials that had NA
nrow(d)
no_missing_vals <- drop_na(d)
nrow(no_missing_vals)

df = subset(stroop_d, drop = rt == "FALSE")
?drop
dropped <- drop(stroop_d$rt == "FALSE")

# there is also a practice condition which might need to be included?
# what is the baseline congruency?
# anyway, analysis
# let's first create the means for all the participants
means <- aggregate(stroop_d$rt,
                         by = list(stroop_d$subject, stroop_d$phase,
                                   stroop_d$congruency),
                         FUN = 'mean')

multi_level_model_intercepts <- lmer(y ~ 1 + x + (x|subject), stroop_d)

# 3 
# load the data
library(readr)
install.packages("factoextra")
library(factoextra)
spotify_cleaned <- read_csv("spotify_cleaned.csv")
library(ggrepel)

# examine the data
summary(spotify_cleaned)
glimpse(spotify_cleaned)

# we still need to clear out some data that is not pertinent to the actual characteristics of the songs
# the data that will be omitted in the PCA are the column number, that is reduntant as r keeps track of that anyway
# the track name, the artist and the number of streams
spotify_for_pca <- select(spotify_cleaned, - "...1", -"Track Name", -"Artist",-"Streams")
pca_spotify <- prcomp(spotify_for_pca)
summary(pca_spotify)
factoextra::fviz_eig(pca_spotify)
# we can see that we replicated the PCA results from the initial analysis of Pratham Nawal
# PC1 explains 64% of the data, and PC2 explains 35% of the data.

fviz_pca_var(pca_spotify, repel = TRUE, alpha.var = 0.5) +
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf)



cor(spotify_for_pca)


#but what happens if we scale it?
pca_spotify_scaled <- prcomp(spotify_for_pca, scale = TRUE)
summary(pca_spotify_scaled)
factoextra::fviz_eig(pca_spotify_scaled)
fviz_contrib(pca_spotify_scaled, choice="var", axes = 1)
fviz_contrib(pca_spotify_scaled, choice="var", axes = 2)

fviz_pca_var(pca_spotify_scaled, repel = TRUE, alpha.var = 0.5)
cor(spotify_for_pca)

# how to improve on their analysis?
# I would inspect which elements of the PC1 and PC2 are actually contributing to the variance
fviz_contrib(pca_spotify, choice="var", axes = 1)
fviz_contrib(pca_spotify, choice="var", axes = 2)
# it seems like in both dimensions, there is only one main component that contributes to the variance of the factor
# so what if we leave only those two in? Meaning, tempo and duration
spotify_for_pca_2 <- select(spotify_cleaned, - "...1", -"Track Name", -"Artist",-"Streams", -"danceability", -"energy", -"key", -"loudness", -"mode", -"speechiness", -"acousticness", -"instrumentalness", -"liveness", -"valence", -"time_signature")
pca_spotify_2 <- prcomp(spotify_for_pca_2)
summary(pca_spotify_2)
fviz_pca_var(pca_spotify_2, repel = TRUE, alpha.var = 0.5)
# well, it seems Alasdair was right
# but what if we scale it too
pca_spotify_2_scaled <- prcomp(spotify_for_pca_2, scale = TRUE)
summary(pca_spotify_2_scaled)
fviz_pca_var(pca_spotify_2_scaled, repel = TRUE, alpha.var = 0.5)

# we can see that the tempo and duration are still significant factors here



spotify_for_pca_2 <- select(spotify_cleaned, - "...1", -"Track Name", -"Artist",-"Streams", -"danceability", -"energy", -"key", -"loudness", -"mode", -"speechiness", -"acousticness", -"instrumentalness", -"liveness", -"valence", -"tempo", -"time_signature", -"duration_sec")
