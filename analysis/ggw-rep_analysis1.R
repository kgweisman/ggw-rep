# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(psych)

# clear environment
rm(list=ls())

# read in anonymized data
d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/pilot-b_01_charmeans.csv")[-1] # get rid of column of obs numbers

glimpse(d)

# --- DATA FORMATTING ---------------------------------------------------------

# make table of character means by mental capacity
charmeans = d %>%
  gather(character, response, -subid, -condition) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T))

glimpse(charmeans)

# format into wideform
charmeans_table = charmeans %>%
  spread(condition, mean)
  
View(charmeans_table)


# --- FACTOR ANALYSIS #1: ORIGINAL GGW2007 _-----------------------------------
# Extract 2 PCs (unrotated)
pca1 = principal(charmeans_table[2:6], nfactors = 2, rotate = "none"); pca1

# Extract 2 PCs (rotated)
pca2 = principal(charmeans_table[2:6], nfactors = 2, rotate = "varimax"); pca2

# Plot PCs against each other for both solutions
par(mfrow=c(1,2))
variables = names(charmeans_table[2:6])
plot(pca1$loadings, type='n')
text(pca1$loadings, labels=variables, cex=.9)
plot(pca2$loadings, type='n')
text(pca2$loadings, labels=variables, cex=.9)


