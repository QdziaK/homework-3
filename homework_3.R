# Homework 3

library(tidyverse)
library(lme4)
stroop_d <- read.csv("stroop_standing_data.csv")

# A within subjects 3 (data type: practice vs congruent vs incongruent) x2 (position: standing vs sitting) Anova can be conducted to analyze whether there is an effect
# of the manipulation of sitting vs standing.

# let's examine the data
summary(stroop_d)

# We need to clean up the data so they are good for the analysis
# it feels like something is missing here in the information that we were given because I do not know what the X is for
# I will drop X as it does not denote anything of interest really
d = subset(stroop_d, select = -c(X) )
# Now that is done, we might want to drop the trials that had NA

# there is also a practice condition which might need to be included?
# anyway, analysis
# let's first create the means for all the participants
means <- aggregate(stroop_d$rt,
                         by = list(stroop_d$subject, stroop_d$phase,
                                   stroop_d$congruency),
                         FUN = 'mean')

multi_level_model_intercepts <- lmer(y ~ 1 + x + (x|subject), stroop_d)


