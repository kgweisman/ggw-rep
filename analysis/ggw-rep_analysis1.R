# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(psych)
library(stats)

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
  
print(charmeans_table)

# make table of mental capacity means by character
# formatted in wideform with characters as rows
condmeans = charmeans %>%
  spread(character, mean)

print(condmeans)

# --- PRINCIPAL COMPONENTS ANALYSIS A: ORIGINAL GGW2007 ----------------------

d1 = charmeans_table[-1]

# Extract 2 PCs (unrotated)
pca_A1 = principal(d1, nfactors = 2, rotate = "none"); pca_A1

# Extract 2 PCs (rotated)
pca_A2 = principal(d1, nfactors = 2, rotate = "varimax"); pca_A2

# Plot PCs against each other for both solutions
par(mfrow=c(1,2))
variables = names(d1)
plot(pca_A1$loadings, type='n')
text(pca_A1$loadings, labels=variables, cex=.9)
plot(pca_A2$loadings, type='n')
text(pca_A2$loadings, labels=variables, cex=.9)

# Extract PCA loadings for PC1 (unrotated)
pc1_A = pca_A1$loadings[,1]

# Extract PCA loadings for PC2 (unrotated)
pc2_A = pca_A1$loadings[,1]

# Set min and max 
m1.pca_A = min(c(pc1_A, pc2_A))
m2.pca_A = max(c(pc1_A, pc2_A))

# Plot participants by principle components (unrotated)
plot.pca_A = plot(pc1_A, pc2_A, xlim = c(m1.pca_A, m2.pca_A)); plot.pca_A
plot.pca_A = plot(pc1_A, pc2_A, xlim = c(m1.pca_A, m2.pca_A)); plot.pca_A

# --- MAXIMUM LIKELIHOOD FACTOR ANALYSIS A -----------------------------------
# Roughly equivalent to PCA #1?

# Factor analysis
fa1 = factanal(d1, 
               factors = 2, 
               rotation = "varimax", 
               na.action = na.omit, 
               scores =  'regression', 
               cutoff = .4)
print(fa1)

# --- HIERARCHICAL CLUSTER ANALYSIS A -----------------------------------------
# Roughly equivalent to PCA #1?

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

# --- Z-SCORE ANALYSES: ORIGINAL GGW2007 --------------------------------------

# NEED TO DO THIS!!

# --- PRINCIPAL COMPONENTS ANALYSIS B -----------------------------------------

d3 = condmeans[-1]

# Extract 2 PCs (unrotated)
pca_B1 = principal(d3, nfactors = 2, rotate = "none"); pca_B1

# Extract 2 PCs (rotated)
pca_B2 = principal(d3, nfactors = 2, rotate = "varimax"); pca_B2

# Plot PCs against each other for both solutions
par(mfrow=c(1,2))
variables = names(d3)
plot(pca_B1$loadings, type='n')
text(pca_B1$loadings, labels=variables, cex=.9)
plot(pca_B2$loadings, type='n')
text(pca_B2$loadings, labels=variables, cex=.9)

# Extract PCA loadings for PC1 (unrotated)
pc_B1 = pca_B1$loadings[,1]

# Extract PCA loadings for PC2 (unrotated)
pc_B2 = pca_B1$loadings[,1]

# Set min and max 
m1.pca_B = min(c(pc_B1, pc_B2))
m2.pca_B = max(c(pc_B1, pc_B2))

# Plot participants by principle components (unrotated)
plot.pca_B = plot(pc_B1, pc_B2, xlim = c(m1.pca_B, m2.pca_B)); plot.pca_B
plot.pca_B = plot(pc_B1, pc_B2, xlim = c(m1.pca_B, m2.pca_B)); plot.pca_B

# --- MULTIDIMENSIONAL SCALING ANALYSES ---------------------------------------

# NOTE: in addition to running for all conditions together (as here), need to filter by condition and run for each condition separately!

upperDissim <- dd %>%
  # alphabetize for upper triangle matrix
  mutate(
    character1 = 
      ifelse(leftCharacter == "charlie_dog" | 
               rightCharacter == "charlie_dog",
             "charlie_dog",
             ifelse(leftCharacter == "delores_gleitman_deceased" |
                      rightCharacter == "delores_gleitman_deceased",
                    "delores_gleitman_deceased",
                    ifelse(leftCharacter == "fetus" |
                             rightCharacter == "fetus",
                           "fetus",
                           ifelse(leftCharacter == "gerald_schiff_pvs" |
                                    rightCharacter == "gerald_schiff_pvs",
                                  "gerald_schiff_pvs",
                                  ifelse(leftCharacter == "god" |
                                           rightCharacter == "god",
                                         "god",
                                         ifelse(leftCharacter == "green_frog" |
                                                  rightCharacter == "green_frog",
                                                "green_frog",
                                                ifelse(leftCharacter == "kismet_robot" |
                                                         rightCharacter == "kismet_robot",
                                                       "kismet_robot",
                                                       ifelse(leftCharacter == "nicholas_gannon_baby" |
                                                                rightCharacter == "nicholas_gannon_baby",
                                                              "nicholas_gannon_baby",
                                                              ifelse(leftCharacter == "samantha_hill_girl" |
                                                                       rightCharacter == "samantha_hill_girl",
                                                                     "samantha_hill_girl",
                                                                     ifelse(leftCharacter == "sharon_harvey_woman" |
                                                                              rightCharacter == "sharon_harvey_woman",
                                                                            "sharon_harvey_woman",
                                                                            ifelse(leftCharacter == "toby_chimp" |
                                                                                     rightCharacter == "toby_chimp",
                                                                                   "toby_chimp",
                                                                                   ifelse(leftCharacter == "todd_billingsley_man" |
                                                                                            rightCharacter == "todd_billingsley_man",
                                                                                          "todd_billingsley_man",
                                                                                          ifelse(leftCharacter == "you" |
                                                                                                   rightCharacter == "you",
                                                                                                 "you",
                                                                                                 "NA")))))))))))))) %>%
  mutate(character2 = ifelse(leftCharacter == character1, as.character(rightCharacter), as.character(leftCharacter))) %>%
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

# rename rows
rows = upperDissim$character1
upperDissim = upperDissim[-1]
rownames(upperDissim) = rows
colnames(upperDissim) = rows

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
quartz()
ggplot(pts, aes(x = x, y = y, label = character, colour = character)) +
  geom_text()+
  theme_bw() +
  theme(legend.position = "none")