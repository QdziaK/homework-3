# Homework 3
options(scipen = 5, digits = 3)
library(tidyverse)
library(lme4)
stroop_d <- read.csv("stroop_standing_data.csv")

# The analysis will be conducted on this within subjects, 2 (data type: congruent vs incongruent) x2 (phase: standing vs sitting) design.

# let's examine the data
summary(stroop_d)
str(stroop_d)
typeof(stroop_d$rt)
stroop_d$rt


# We need to clean up the data so they are good for the analysis
# I will drop the X and correct columns as it does not denote anything of interest for the analysis.
# I can also drop the baseline congruency and practice phase, as these will not be analyzed.
# Additionally, for some reason the rt column contains "FALSE" values, so that will need to be filtered as well. It is also apparently a character .

stroop_filtered <- stroop_d %>%
  subset(select = -c(X, correct)) %>%
  filter(congruency != "baseline", phase != "practice", rt != "FALSE")  %>%
  transform(rt = as.numeric(rt))

typeof(stroop_filtered$rt)
is.numeric(stroop_filtered$rt)


# Now that is done, we might want to drop the trials that had NA in
nrow(stroop_filtered)
stroop_no_na <- drop_na(stroop_filtered)
nrow(stroop_no_na)


# and now, analysis
# let's first create the means for all the conditions, to see if the data works well
stroop_means <- aggregate(stroop_no_na$rt,
                         by = list(stroop_no_na$phase,
                                   stroop_no_na$congruency),
                         FUN = 'mean')
stroop_means

ggplot(stroop_no_na) +
  aes(x = phase, y = rt) +
  geom_boxplot() +
  facet_wrap(~congruency)


# Now to the analysis
str(stroop_no_na)

# We have reaction time as the dependent variable.
# As fixed effects, there is phase (standing vs sitting), and there is congruency (congruent vs incongruent)
# And then there are the subjects, which are the random effects.
# We want to model the effect of phase and congruency on the reaction time, and we want to see if there is an interaction between phase and congruency.
# Also, we would want a intercept and slope for each subject that varies by the conditions.
stroop_model <- lmer(rt ~ phase * congruency + (1|subject) + (1|phase:subject) + (1|congruency:subject), stroop_no_na)
summary(stroop_model)

# We have fit the model but I am not entirely sure how to analyze the results.



model <- aov(rt ~ phase *congruency, data = stroop_no_na)
summary(model)



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
