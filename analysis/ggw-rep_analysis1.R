# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(psych)
library(stats)
library(scales)

# clear environment
rm(list=ls())

# read in data: character means
d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/pilot-b_01_charmeans.csv")[-1] # get rid of column of obs numbers

glimpse(d)

# read in data: individual scores
dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/pilot-b_01_data_anonymized.csv")[-1] # get rid of column of obs numbers

glimpse(dd)

# --- DATA FORMATTING ---------------------------------------------------------

# make table of character means by mental capacity
charmeans = d %>%
  gather(character, response, -subid, -condition) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T))

glimpse(charmeans)

# format into wideform with characters as rows
charmeans_table = charmeans %>%
  spread(condition, mean)

rows = charmeans_table$character
d1 = charmeans_table[-1]
rownames(d1) = rows
names(d1) = c("Consciousness", "EmotionRecognition", "Hunger", "Pain", "Rage")
print(d1)

# make table of mental capacity means by character
# formatted in wideform with characters as rows
condmeans = charmeans %>%
  spread(character, mean)

rows = condmeans$condition
d3 = condmeans[-1]
rownames(d3) = rows
print(d3)

# --- PRINCIPAL COMPONENTS ANALYSIS A: ORIGINAL GGW2007 ----------------------

# NOTES: 
# - could also look at unrotated solution by specifying rotate = "none"
# - should also look at other numbers of factors when we have more data

# --------> 1-factor PCA (varimax rotation, using principal) ----------
# extract factors
pca_A1 = principal(d1, nfactors = 1, rotate = "varimax"); pca_A1

# extract PCA loadings
pca_A1_pc1 = pca_A1$loadings[,1]

# --------> 2-factor PCA (varimax rotation, using principal) ----------
# extract factors
pca_A2 = principal(d1, nfactors = 2, rotate = "varimax"); pca_A2

# extract PCA loadings
pca_A2_pc1 = pca_A2$loadings[,1]
pca_A2_pc2 = pca_A2$loadings[,2]

# plot PCs against each other
# NOTE: need to adjust "1:5" to "1:18" if all 18 conditions are run
ggplot(data.frame(pca_A2$loadings[1:5,]), aes(x = RC2, y = RC1, label = names(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings\n",
       x = "\nRotated PC2",
       y = "Rotated PC1\n")

# plot characters by principle components, PC1 on y-axis
ggplot(data.frame(pca_A2$scores), aes(x = RC2, y = RC1, label = rownames(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Raw character factor scores\n",
       x = "\nRotated PC2",
       y = "Rotated PC1\n")

# re-plot characters with rescaling (as in GGW2007 original), PC1 on y-axis
ggplot(data.frame(pca_A2$scores), aes(x = rescale(RC2, to = c(0,1)), y = rescale(RC1, to = c(0,1)), label = rownames(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Adjusted character factor scores\n",
       x = "\nRotated PC2 (rescaled)",
       y = "Rotated PC1 (rescaled)\n")

# --- Z-SCORE ANALYSES: ORIGINAL GGW2007 --------------------------------------

# NEED TO DO THIS!!

# --- PRINCIPAL COMPONENTS ANALYSIS B -----------------------------------------

# NOTES: 
# - in addition to running for all conditions together (as here), need to filter by condition and run for each condition separately!
# - could also look at unrotated solution by specifying rotate = "none"
# - should also look at other numbers of factors when we have more data

# --------> 1-factor PCA (varimax rotation, using principal) ----------
# extract factors
pca_B1 = principal(d3, nfactors = 1, rotate = "varimax"); pca_B1

# extract PCA loadings
pca_B1_pc1 = pca_B1$loadings[,1]

# --------> 2-factor PCA (varimax rotation, using principal) ----------
# extract factors
pca_B2 = principal(d3, nfactors = 2, rotate = "varimax"); pca_B2

# extract PCA loadings
pca_B2_pc1 = pca_B2$loadings[,1]
pca_B2_pc2 = pca_B2$loadings[,2]

# plot PCs against each other
ggplot(data.frame(pca_B2$loadings[1:13,]), aes(x = RC2, y = RC1, label = names(d3))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings\n",
       x = "\nRotated PC2",
       y = "Rotated PC1\n")

# plot characters by principle components, PC1 on y-axis
ggplot(data.frame(pca_B2$scores), aes(x = RC2, y = RC1, label = rownames(d3))) +
  geom_text() +
  theme_bw() +
  labs(title = "Raw condition factor scores\n",
       x = "\nRotated PC2",
       y = "Rotated PC1\n")

# re-plot characters with rescaling (as in GGW2007 original), PC1 on y-axis
ggplot(data.frame(pca_B2$scores), aes(x = rescale(RC2, to = c(0,1)), y = rescale(RC1, to = c(0,1)), label = rownames(d3))) +
  geom_text() +
  theme_bw() +
  labs(title = "Adjusted condition factor scores\n",
       x = "\nRotated PC2 (rescaled)",
       y = "Rotated PC1 (rescaled)\n")

# --- MULTIDIMENSIONAL SCALING ANALYSES ---------------------------------------

# NOTE: in addition to running for all conditions together (as here), need to filter by condition and run for each condition separately!

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
upperDissim <- dd %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(upperDissim$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  upperDissim <- upperDissim %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
upperDissim <- upperDissim %>%
  select(subid, condition, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
upperDissim$charlie_dog = NA
upperDissim[13,] = c("you", rep(NA, 13))

# reorder columns
upperDissim = upperDissim[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
upperDissim = upperDissim[-1]
rownames(upperDissim) = names
colnames(upperDissim) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    upperDissim[j,i] = upperDissim[i,j]
  }
}

# replace NAs with 0 and convert to numeric
for(i in 1:13) {
  upperDissim[i,i] = 0
}

# Convert to numeric matrix form 
upperDissim = data.matrix(upperDissim)

# Do MDS, pull out x and y coords
fit <- cmdscale(upperDissim, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]

# Convert to a dataframe and join in category labels
pts <- data.frame(x = x, y = y, character = row.names(upperDissim))
pts <- left_join(pts, row.names(upperDissim))

# Plot!
ggplot(pts, aes(x = x, y = y, label = character)) +
  geom_text() +
  theme_bw() +
  labs(title = "Multidimensional scaling of characters\n",
       x = NULL,
       y = NULL)

# --- MAXIMUM LIKELIHOOD FACTOR ANALYSIS A -----------------------------------
# Roughly equivalent to pca_A?
# Could also do the parallel version of pca_B

# Factor analysis
fa1 = factanal(d1, 
               factors = 2, 
               rotation = "varimax", 
               na.action = na.omit, 
               scores =  'regression', 
               cutoff = .4)
print(fa1)

# --- HIERARCHICAL CLUSTER ANALYSIS A -----------------------------------------
# Roughly equivalent to pca_A
# Could also do the parallel version of pca_B

# Construct dissimilarity matrix
d2 = as.dist((1-cor(d1))/2)

# Conduct hierarchical cluster analysis
hca = hclust(d2); hca

# Plot dendogram
par(mfrow=c(1,2))
rs1=hclust(d2)
rs1$merge
plot(rs1$height)
plot(rs1)


