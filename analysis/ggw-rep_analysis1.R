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
pca_A1_pc1 = pca_A1$loadings[,1]; pca_A1_pc1

# --------> 2-factor PCA (varimax rotation, using principal) ----------

# FROM GGW2007: "For each survey, each character appeared in 12 different comparisons, and mean relative ratings were computed for each character across all respondents to that survey. We merged data sets from the 18 mental capacity surveys to compute correlations between mental capacities across the characters, and submitted these to principal components factor analysis with varimax rotation." (SOM p. 3)

# extract factors
pca_A2 = principal(d1, nfactors = 2, rotate = "varimax"); pca_A2

# extract PCA loadings
pca_A2_pc1 = pca_A2$loadings[,1]; pca_A2_pc1
pca_A2_pc2 = pca_A2$loadings[,2]; pca_A2_pc2

# plot PCs against each other
# NOTE: need to adjust "1:5" to "1:18" if all 18 conditions are run
ggplot(data.frame(pca_A2$loadings[1:5,]), aes(x = RC2, y = RC1, label = names(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings\n",
       x = "\nRotated Component 2",
       y = "Rotated Component 1\n")

# FROM GGW2007: "We used the regression approach to estimate factor scores for each character." (SOM p. 3) 
# ?principal confirms that "component scores are found by regression"

# plot characters by principle components, PC1 on y-axis
ggplot(data.frame(pca_A2$scores), aes(x = RC2, y = RC1, label = rownames(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Raw character factor scores\n",
       x = "\nRotated Component 2",
       y = "Rotated Component 1\n")

# FROM GGW2007: "For ease of interpretation, factor scores in Figure 1 were adjusted to be anchored at 0 and 1" (SOM p. 3)

# re-plot characters with rescaling (as in GGW2007 original), PC1 on y-axis
ggplot(data.frame(pca_A2$scores), aes(x = rescale(RC2, to = c(0,1)), y = rescale(RC1, to = c(0,1)), label = rownames(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Adjusted character factor scores\n",
       x = "\nRotated Component 2 (rescaled)",
       y = "Rotated Component 1 (rescaled)\n")

# --- Z-SCORE ANALYSES: ORIGINAL GGW2007 --------------------------------------

# NEED TO DO THIS!!

# from GGW2007: "We examined the role of individual-difference variables by partitioning respondents according to 9 variables: gender, age, strength of religious beliefs, attainment of college education, political affiliation (Democrat or Republican), marital status, parental status, dog ownership, and strength of belief in a spiritual afterlife. Median splits were made for the 3 continuous variables (age, strength of religious beliefs, and belief in a spiritual afterlife) so that all variables had 2 levels. We then computed means for each character for each level of the individual-difference variable (e.g., men versus women). We used factor score coefficients from the omnibus factor analysis to estimate scores on Experience and Agency separately for each group, and then calculated the difference in factor scores between the two groups. We divided the difference scores by the standard error for the difference to produce z-statistics” (SOM p. 4)."

# --------> gender ------------------------------------------------------------

# make two dataframes
dgen_f = dd %>% filter(gender == "female")
dgen_m = dd %>% filter(gender == "male")



# --------> age ---------------------------------------------------------------

# make two dataframes
dage_young = dd %>% filter(age < median(age))
dage_old = dd %>% filter(age > median(age))



# --------> religious beliefs -------------------------------------------------

# make two dataframes
drelig_noGod = dd %>% 
  mutate(beliefGod_num = 
           ifelse(beliefGod == "disagree_strong", -3,
                  ifelse(beliefGod == "disagree_moderate", -2,
                         ifelse(beliefGod == "disagree_little", -1,
                                ifelse(beliefGod == "neither", 0,
                                       ifelse(beliefGod == "agree_litte", 1,
                                              ifelse(beliefGod == "agree_moderate", 2,
                                                     ifelse(beliefGod == "agree_strong", 3,
                                                            NA)))))))) %>%
  filter(beliefGod_num < median(beliefGod_num))
drelig_yesGod = dd %>% 
  mutate(beliefGod_num = 
           ifelse(beliefGod == "disagree_strong", -3,
                  ifelse(beliefGod == "disagree_moderate", -2,
                         ifelse(beliefGod == "disagree_little", -1,
                                ifelse(beliefGod == "neither", 0,
                                       ifelse(beliefGod == "agree_litte", 1,
                                              ifelse(beliefGod == "agree_moderate", 2,
                                                     ifelse(beliefGod == "agree_strong", 3,
                                                            NA)))))))) %>%
  filter(beliefGod_num > median(beliefGod_num))



# --------> education ---------------------------------------------------------

# make two dataframes
dedu_noCollegeDeg = dd %>%
  mutate(education_split =
           ifelse(education == "hs_none" |
                    education == "hs_some" |
                    education == "hs_diploma" |
                    education == "college_some",
                  "noCollegeDegree",
                  ifelse(education == "college_assocDegree" |
                           education == "college_bachDegree" | 
                           education == "grad_some" |
                           education == "grad_degree",
                         "yesCollegeDegree",
                         NA)
                  )) %>%
  filter(education_split == "noCollegeDegree")
dedu_yesCollegeDeg = dd %>%
  mutate(education_split =
           ifelse(education == "hs_none" |
                    education == "hs_some" |
                    education == "hs_diploma" |
                    education == "college_some",
                  "noCollegeDegree",
                  ifelse(education == "college_assocDegree" |
                           education == "college_bachDegree" | 
                           education == "grad_some" |
                           education == "grad_degree",
                         "yesCollegeDegree",
                         NA)
           )) %>%
  filter(education_split == "yesCollegeDegree")



# --------> political affiliation ---------------------------------------------

# make two dataframes
dpol_democrat = dd %>%
  filter(politicalIdeology == "democrat")
dpol_republican = dd %>%
  filter(politicalIdeology == "republican")



# --------> marital status ----------------------------------------------------

# make two dataframes
dmar_noMarriage = dd %>%
  filter(maritalStatus == "yes")
dmar_yesMarriage = dd %>%
  filter(maritalStatus == "no_committed" |
           maritalStatus == "no")



# --------> parental status ---------------------------------------------------

# make two dataframes
dpar_noChild = dd %>%
  filter(children == 0)
dpar_yesChild = dd %>%
  filter(children > 0)



# --------> dog ownership -----------------------------------------------------

# make two dataframes
ddog_noDog = dd %>%
  filter(dog == "yes")
ddog_yesDog = dd %>%
  filter(dog == "no")



# --------> belief in spiritual afterlife -------------------------------------

# make two dataframes
dgen_noAfterlife = dd %>% 
  mutate(beliefAfterlife_num = 
           ifelse(beliefAfterlife == "disagree_strong", -3,
                  ifelse(beliefAfterlife == "disagree_moderate", -2,
                         ifelse(beliefAfterlife == "disagree_little", -1,
                                ifelse(beliefAfterlife == "neither", 0,
                                       ifelse(beliefAfterlife == "agree_litte", 1,
                                              ifelse(beliefAfterlife == "agree_moderate", 2,
                                                     ifelse(beliefAfterlife == "agree_strong", 3,
                                                            NA)))))))) %>%
  filter(beliefAfterlife_num < median(beliefAfterlife_num))
dgen_yesAfterlife = dd %>% 
  mutate(beliefAfterlife_num = 
           ifelse(beliefAfterlife == "disagree_strong", -3,
                  ifelse(beliefAfterlife == "disagree_moderate", -2,
                         ifelse(beliefAfterlife == "disagree_little", -1,
                                ifelse(beliefAfterlife == "neither", 0,
                                       ifelse(beliefAfterlife == "agree_litte", 1,
                                              ifelse(beliefAfterlife == "agree_moderate", 2,
                                                     ifelse(beliefAfterlife == "agree_strong", 3,
                                                            NA)))))))) %>%
  filter(beliefAfterlife_num > median(beliefAfterlife_num))



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

# --------> MDS 1: all conditions ---------------------------------------------

# ----------------> data formatting -------------------------------------------
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

# ----------------> do MDS ----------------------------------------------------
# do MDS, pull out x_all and y_all coords
fit_all <- cmdscale(upperDissim, eig = TRUE, k = 2)
x_all <- fit_all$points[, 1]
y_all <- fit_all$points[, 2]

# convert to a dataframe
pts <- data.frame(x = x_all, y = y_all, character = row.names(upperDissim))

# plot!
ggplot(pts, aes(x = x_all, y = y_all, label = character)) +
  geom_text() +
  theme_bw() +
  labs(title = "Multidimensional scaling of characters:\nAll conditions\n",
       x = NULL,
       y = NULL)

# --------> MDS 2: each condition separately ----------------------------------

# ----------------> data formatting & MDS -------------------------------------

for(k in 1:length(levels(dd$condition))) {
  condition_temp = levels(dd$condition)[k]
  
  upperDissim_temp = NULL
  fit_temp = NULL
  x_temp = NULL
  y_temp = NULL
  pts_temp = NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  upperDissim_temp <- dd %>%
    filter(condition == condition_temp) %>%
    mutate(character1 = array(),
           character2 = array())
  
  charsort = sort(levels(upperDissim_temp$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    upperDissim_temp <- upperDissim_temp %>%
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
  upperDissim_temp <- upperDissim_temp %>%
    select(subid, condition, character1, character2, responseNum) %>%
    group_by(character1, character2) %>%
    mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
    summarise(mean = mean(dist, na.rm = TRUE)) %>%
    spread(character2, mean)
  
  # add in NA column for charlie_dog, NA row for you
  upperDissim_temp <- upperDissim_temp %>%
    mutate(charlie_dog = NA,
           character1 = as.character(character1)) %>%
    rbind(c("you", rep(NA, 13))) %>%
    mutate(character1 = factor(character1))
  
  # reorder columns
  upperDissim_temp = upperDissim_temp[, c(1, 14, 2:13)]
  
  # rename rows and columns
  names = sort(charsort, decreasing = FALSE)
  upperDissim_temp = upperDissim_temp[-1]
  rownames(upperDissim_temp) = names
  colnames(upperDissim_temp) = names
  
  # fill in lower triangle matrix
  for(i in 1:12) {
    for(j in (i+1):13) {
      upperDissim_temp[j,i] = upperDissim_temp[i,j]
    }
  }
  
  # replace NAs with 0 and convert to numeric
  for(i in 1:13) {
    upperDissim_temp[i,i] = 0
  }
  
  # Convert to numeric matrix form 
  upperDissim_temp = data.matrix(upperDissim_temp)
  
  # do MDS, pull out x_temp and y_temp coords
  fit_temp <- cmdscale(upperDissim_temp, eig = TRUE, k = 2)
  x_temp <- fit_temp$points[, 1]
  y_temp <- fit_temp$points[, 2]
  
  # convert to a dataframe and join in category_temp labels
  pts_temp <- data.frame(x = x_temp, y = y_temp, character = row.names(upperDissim_temp))
  
  # plot!
  plot = 
    ggplot(pts_temp, aes(x = x_temp, y = y_temp, label = character)) +
      geom_text() +
      theme_bw() +
      labs(title = paste0("Multidimensional scaling of characters:\n",condition_temp,"\n"),
           x = NULL,
           y = NULL)
  print(plot)
}

# --- ADDITIONAL ALTERNATIVE ANALYES ------------------------------------------

# --------> MAXIMUM LIKELIHOOD FACTOR ANALYSIS A ------------------------------
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

# --------> HIERARCHICAL CLUSTER ANALYSIS A -----------------------------------
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
