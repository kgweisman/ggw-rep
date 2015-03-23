########################################################### preliminaries #####

# --- PACKAGES & FUNCTIONS ----------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scatterplot3d)
library(lme4)
library(psych)
library(stats)
library(scales)
library(smacof)

# clear environment
rm(list=ls())

# clear graphics
dev.off()

# define multiplot function
# source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# --- IMPORTING DATA ----------------------------------------------------------

# read in data: character means
d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/run-01_2015-03-09_charmeans.csv")[-1] # get rid of column of obs numbers

glimpse(d)

# read in data: individual scores
dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/run-01_2015-03-09_data_anonymized.csv")[-1] # get rid of column of obs numbers

glimpse(dd)

# --- FILTERING DATA ----------------------------------------------------------

# # filter out trials where log_rt < 2SDs below mean
# dd = dd %>%
#   filter(under_lower == 0)
# 
# View(dd %>% group_by(subid) %>% summarise(trials_completed = length(log_rt)))

# # filter out participants where >50% trials have log_rt < 2SDs below mean
# dd = dd %>%
#   filter(prop_under > .5)
# 
# View(dd %>% group_by(subid) %>% summarise(trials_completed = length(log_rt)))

# --- FORMATTING DATA ---------------------------------------------------------

# make table of character means by mental capacity
charmeans = d %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T))

glimpse(charmeans)

# format into wideform with characters as rows
charmeans_table = charmeans %>%
  spread(condition, mean)

charnames = as.character(charmeans_table$character)
charnames = ifelse(charnames == "charlie_dog", "dog",
              ifelse(charnames == "delores_gleitman_deceased", "dead woman",
                     ifelse(charnames == "gerald_schiff_pvs", "PVS man", 
                            ifelse(charnames == "green_frog", "frog",
                                   ifelse(charnames == "samantha_hill_girl", "girl",
                                          ifelse(charnames == "kismet_robot", "robot",
                                                 ifelse(charnames == "nicholas_gannon_baby", "baby",
                                                        ifelse(charnames == "sharon_harvey_woman", "woman",
                                                               ifelse(charnames == "toby_chimp", "chimp",
                                                                      ifelse(charnames == "todd_billingsley_man", "man",
                                                                             as.character(charnames)))))))))))

d1 = charmeans_table[-1]
rownames(d1) = charnames
print(d1)

# make table of mental capacity means by character
# formatted in wideform with characters as rows
condmeans = d %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, subid, character) %>%
  summarise(mean = mean(response, na.rm = T))

glimpse(condmeans)

# format into wideform with characters as rows
condmeans_table = condmeans %>%
  spread(character, mean)

subidnames = condmeans_table$subid

d3 = condmeans_table[-1]
d3 = d3[-1]
names(d3) = charnames
rownames(d3) = subidnames
print(d3)

########################################################### summary stats #####

# --- DEMOGRAPHICS ------------------------------------------------------------

# total n
dd %>% distinct(subid) %>% summarise(n = length(subid))

# condition assignment
dd %>% group_by(condition) %>% distinct(subid) %>% summarise(n = length(subid))

# gender
dd %>% distinct(subid) %>% count(gender)

# ethnicity
dd %>% distinct(subid) %>% count(ethnicity)

# education
dd %>% distinct(subid) %>% count(education)

# englishNative
dd %>% distinct(subid) %>% count(englishNative)

# politicalIdeology
dd %>% distinct(subid) %>% count(politicalIdeology)

# religionChild
dd %>% distinct(subid) %>% count(religionChild)

# maritalStatus
dd %>% distinct(subid) %>% count(maritalStatus)

################################################### analysis & plots pt 1 #####

# --- PRINCIPAL COMPONENTS ANALYSIS A: ORIGINAL GGW2007 ----------------------

# NOTES: 
# - should look again at >2 factors when we have more data
# - good resource: http://www.colorado.edu/geography/class_homepages/geog_4023_s11/Lecture18_PCA.pdf

# --------> 4-factor (maximal) PCA (UNrotated, using principal) ----------

# extract factors
pca_A4 = principal(d1, nfactors = 4, rotate = "none"); pca_A4
# retain 2 components (prop var > 5-10%)
# retain 1-2 components? (cumulative prop var > 70%... but < 100%?)

# extract eigenvalues
pca_A4$values # retain 2 components (eigenvalue > 1)

# scree test
qplot(y = pca_A4$values) +
  theme_bw() +
  labs(title = "Scree test for 4-factor (maximal) PCA",
       x = "Component",
       y = "Eigenvalue") +
  geom_line() # retain 2-3 components (left of "break")

# extract PCA loadings
pca_A4_pc1 = pca_A4$loadings[,1]; sort(pca_A4_pc1)
pca_A4_pc2 = pca_A4$loadings[,2]; sort(pca_A4_pc2)
pca_A4_pc3 = pca_A4$loadings[,3]; sort(pca_A4_pc3)
pca_A4_pc4 = pca_A4$loadings[,4]; sort(pca_A4_pc4)

# --------> 2-factor PCA (varimax rotation, using principal) ----------

# FROM GGW2007: "For each survey, each character appeared in 12 different comparisons, and mean relative ratings were computed for each character across all respondents to that survey. We merged data sets from the 18 mental capacity surveys to compute correlations between mental capacities across the characters, and submitted these to principal components factor analysis with varimax rotation." (SOM p. 3)

# extract factors
pca_A2 = principal(d1, nfactors = 2, rotate = "varimax"); pca_A2

# extract eigenvalues
pca_A2$values

# extract PCA loadings
pca_A2_pc1 = pca_A2$loadings[,1]; sort(pca_A2_pc1)
pca_A2_pc2 = pca_A2$loadings[,2]; sort(pca_A2_pc2)

# --------------->-> plots ----------------------------------------------------

# plot PCs against each other
# NOTE: need to adjust "1:4" depending on how many conditions are run
ggplot(data.frame(pca_A2$loadings[1:4,]), aes(x = RC1, y = RC2, label = names(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings\n",
       x = "\nRotated Component 2",
       y = "Rotated Component 1\n")

# FROM GGW2007: "We used the regression approach to estimate factor scores for each character." (SOM p. 3) 
# ?principal confirms that "component scores are found by regression"

# plot characters by principle components
ggplot(data.frame(pca_A2$scores), aes(x = RC1, y = RC2, label = rownames(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Raw character factor scores\n",
       x = "\nRotated Component 1: 'Agency'",
       y = "Rotated Component 2: 'Experience'\n")

# FROM GGW2007: "For ease of interpretation, factor scores in Figure 1 were adjusted to be anchored at 0 and 1" (SOM p. 3)

# re-plot characters with rescaling (as in GGW2007 original), PC1 on y-axis
ggplot(data.frame(pca_A2$scores), 
       aes(x = rescale(RC1, to = c(0,1)), 
           y = rescale(RC2, to = c(0,1)), 
           label = rownames(d1))) +
  geom_point() +
  geom_text(angle = 0,
            vjust = -1,
            size = 6) +
  xlim(-0.01, 1.01) +
  ylim(-0.01, 1.01) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Adjusted character factor scores\n",
       x = "\nRotated Component 1, rescaled: 'Agency'",
       y = "Rotated Component 2, rescaled: 'Experience'\n")

# --------> 1-factor PCA (varimax rotation, using principal) ----------
# extract factors
pca_A1 = principal(d1, nfactors = 1, rotate = "varimax"); pca_A1

# extract PCA loadings
pca_A1_pc1 = pca_A1$loadings[,1]; sort(pca_A1_pc1)

# --- Z-SCORE ANALYSES: ORIGINAL GGW2007 *** IN PROGRESS*** -------------------

# from GGW2007 (SOM p. 4):
# - "We examined the role of individual-difference variables by partitioning respondents according to 9 variables: gender, age, strength of religious beliefs, attainment of college education, political affiliation (Democrat or Republican), marital status, parental status, dog ownership, and strength of belief in a spiritual afterlife. Median splits were made for the 3 continuous variables (age, strength of religious beliefs, and belief in a spiritual afterlife) so that all variables had 2 levels. 
# - "We then computed means for each character for each level of the individual-difference variable (e.g., men versus women). 
# - "We used factor score coefficients from the omnibus factor analysis to estimate scores on Experience and Agency separately for each group...
# - "...and then calculated the difference in factor scores between the two groups. 
# - "We divided the difference scores by the standard error for the difference to produce z-statistics"

# --------> factor score coefficients -----------------------------------------

# component 1 (agency)
pca_A2_pc1

# component 2 (experience)
pca_A2_pc2

# --------> gender ------------------------------------------------------------

n_f = as.numeric(d %>% 
                   filter(gender == "female") %>% 
                   select(subid) %>% 
                   distinct() %>% 
                   count())

n_m = as.numeric(d %>% 
                   filter(gender == "male") %>% 
                   select(subid) %>% 
                   distinct() %>% 
                   count())

print(c(f = n_f, m = n_m))

# make charmeans_table for women
charmeans_genf_table = d %>%
  filter(gender == "female") %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(condition, mean) %>%
  rename(mean_fear = Fear,
         mean_hunger = Hunger,
         mean_morality = Morality,
         mean_selfcontrol = SelfControl)

# make charsds_table for women
charsds_genf_table = d %>%
  filter(gender == "female") %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, character) %>%
  summarise(sd = sd(response, na.rm = T)) %>%
  spread(condition, sd) %>%
  rename(sd_fear = Fear,
         sd_hunger = Hunger,
         sd_morality = Morality,
         sd_selfcontrol = SelfControl)

# multiply MC scores by pca-A2 factor loadings
d0_genf = charmeans_genf_table %>%
  full_join(charsds_genf_table) %>%
  mutate(mean_pc1_f = mean_fear * pca_A2_pc1["Fear"] +
           mean_hunger * pca_A2_pc1["Hunger"] +
           mean_morality * pca_A2_pc1["Morality"] +
           mean_selfcontrol * pca_A2_pc1["SelfControl"],
         mean_pc2_f = mean_fear * pca_A2_pc2["Fear"] +
           mean_hunger * pca_A2_pc2["Hunger"] +
           mean_morality * pca_A2_pc2["Morality"] +
           mean_selfcontrol * pca_A2_pc2["SelfControl"],
         sd_pc_f = sd_fear + sd_hunger + sd_morality + sd_selfcontrol)

d1_genf = select(d0_genf, character, mean_pc1_f, mean_pc2_f, sd_pc_f)
print(d1_genf)

# make charmeans_table for men
charmeans_genm_table = d %>%
  filter(gender == "male") %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(condition, mean) %>%
  rename(mean_mear = Fear,
         mean_hunger = Hunger,
         mean_morality = Morality,
         mean_selfcontrol = SelfControl)

# make charsds_table for men
charsds_genm_table = d %>%
  filter(gender == "male") %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, character) %>%
  summarise(sd = sd(response, na.rm = T)) %>%
  spread(condition, sd) %>%
  rename(sd_mear = Fear,
         sd_hunger = Hunger,
         sd_morality = Morality,
         sd_selfcontrol = SelfControl)

# multiply MC scores by pca-A2 factor loadings
d0_genm = charmeans_genm_table %>%
  full_join(charsds_genm_table) %>%
  mutate(mean_pc1_m = mean_mear * pca_A2_pc1["Fear"] +
           mean_hunger * pca_A2_pc1["Hunger"] +
           mean_morality * pca_A2_pc1["Morality"] +
           mean_selfcontrol * pca_A2_pc1["SelfControl"],
         mean_pc2_m = mean_mear * pca_A2_pc2["Fear"] +
           mean_hunger * pca_A2_pc2["Hunger"] +
           mean_morality * pca_A2_pc2["Morality"] +
           mean_selfcontrol * pca_A2_pc2["SelfControl"],
         sd_pc_m = sd_mear + sd_hunger + sd_morality + sd_selfcontrol)

d1_genm = select(d0_genm, character, mean_pc1_m, mean_pc2_m, sd_pc_m)
print(d1_genm)

# look at z-scores of differences
d1_gen = full_join(d1_genf, d1_genm) %>%
  mutate(pc1_FMdiff = mean_pc1_f - mean_pc1_m,
         pc2_FMdiff = mean_pc2_f - mean_pc2_m,
         sd_FMdiff = sqrt((sd_pc_f^2)/n_f + (sd_pc_m^2)/n_m)) %>%
  select(character, pc1_FMdiff, pc2_FMdiff, sd_FMdiff) %>%
  mutate(pc1_FMz = pc1_FMdiff/sd_FMdiff,
         pc2_FMz = pc2_FMdiff/sd_FMdiff,
         pc1_FMp = pnorm(pc1_FMz),
         pc2_FMp = pnorm(pc2_FMz),
         pc1_signif = ifelse(pc1_FMp < 0.001, "***", 
                             ifelse(pc1_FMp < 0.01, "**",
                                    ifelse(pc1_FMp < 0.05, "*",
                                           ifelse(pc1_FMp < 0.1, "m",
                                                  "ns")))),
         pc2_signif = ifelse(pc2_FMp < 0.001, "***", 
                             ifelse(pc2_FMp < 0.01, "**",
                                    ifelse(pc2_FMp < 0.05, "*",
                                           ifelse(pc2_FMp < 0.1, "m",
                                                  "ns"))))) %>%
  select(character, pc1_FMz, pc1_FMp, pc1_signif, pc2_FMz, pc2_FMp, pc2_signif)
d1_gen

# --------> age ------------------------------------------------------------

n_young = as.numeric(d %>%
                       filter(age != "NA") %>%
                       filter(age <= median(age)) %>%
                       select(subid) %>% 
                       distinct() %>% 
                       count())

n_old = as.numeric(d %>%
                       filter(age != "NA") %>%
                       filter(age > median(age)) %>%
                       select(subid) %>% 
                       distinct() %>% 
                       count())

print(c(young = n_young, old = n_old))

# make charmeans_table for women
charmeans_ageyoung_table = d %>%
  filter(age != "NA") %>%
  filter(age <= median(age)) %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(condition, mean) %>%
  rename(mean_youngear = Fear,
         mean_hunger = Hunger,
         mean_morality = Morality,
         mean_selfcontrol = SelfControl)

# make charsds_table for women
charsds_ageyoung_table = d %>%
  filter(age != "NA") %>%
  filter(age <= median(age)) %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, character) %>%
  summarise(sd = sd(response, na.rm = T)) %>%
  spread(condition, sd) %>%
  rename(sd_youngear = Fear,
         sd_hunger = Hunger,
         sd_morality = Morality,
         sd_selfcontrol = SelfControl)

# multiply MC scores by pca-A2 factor loadings
d0_ageyoung = charmeans_ageyoung_table %>%
  full_join(charsds_ageyoung_table) %>%
  mutate(mean_pc1_young = mean_youngear * pca_A2_pc1["Fear"] +
           mean_hunger * pca_A2_pc1["Hunger"] +
           mean_morality * pca_A2_pc1["Morality"] +
           mean_selfcontrol * pca_A2_pc1["SelfControl"],
         mean_pc2_young = mean_youngear * pca_A2_pc2["Fear"] +
           mean_hunger * pca_A2_pc2["Hunger"] +
           mean_morality * pca_A2_pc2["Morality"] +
           mean_selfcontrol * pca_A2_pc2["SelfControl"],
         sd_pc_young = sd_youngear + sd_hunger + sd_morality + sd_selfcontrol)

d1_ageyoung = select(d0_ageyoung, character, mean_pc1_young, mean_pc2_young, sd_pc_young)
print(d1_ageyoung)

# make charmeans_table for men
charmeans_ageold_table = d %>%
  filter(age != "NA") %>%
  filter(age > median(age)) %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(condition, mean) %>%
  rename(mean_oldear = Fear,
         mean_hunger = Hunger,
         mean_morality = Morality,
         mean_selfcontrol = SelfControl)

# make charsds_table for men
charsds_ageold_table = d %>%
  filter(age != "NA") %>%
  filter(age > median(age)) %>%  
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
  group_by(condition, character) %>%
  summarise(sd = sd(response, na.rm = T)) %>%
  spread(condition, sd) %>%
  rename(sd_oldear = Fear,
         sd_hunger = Hunger,
         sd_morality = Morality,
         sd_selfcontrol = SelfControl)

# multiply MC scores by pca-A2 factor loadings
d0_ageold = charmeans_ageold_table %>%
  full_join(charsds_ageold_table) %>%
  mutate(mean_pc1_old = mean_oldear * pca_A2_pc1["Fear"] +
           mean_hunger * pca_A2_pc1["Hunger"] +
           mean_morality * pca_A2_pc1["Morality"] +
           mean_selfcontrol * pca_A2_pc1["SelfControl"],
         mean_pc2_old = mean_oldear * pca_A2_pc2["Fear"] +
           mean_hunger * pca_A2_pc2["Hunger"] +
           mean_morality * pca_A2_pc2["Morality"] +
           mean_selfcontrol * pca_A2_pc2["SelfControl"],
         sd_pc_old = sd_oldear + sd_hunger + sd_morality + sd_selfcontrol)

d1_ageold = select(d0_ageold, character, mean_pc1_old, mean_pc2_old, sd_pc_old)
print(d1_ageold)

# look at z-scores of differences
d1_age = full_join(d1_ageyoung, d1_ageold) %>%
  mutate(pc1_youngolddiff = mean_pc1_young - mean_pc1_old,
         pc2_youngolddiff = mean_pc2_young - mean_pc2_old,
         sd_youngolddiff = sqrt((sd_pc_young^2)/n_young + (sd_pc_old^2)/n_old)) %>%
  select(character, pc1_youngolddiff, pc2_youngolddiff, sd_youngolddiff) %>%
  mutate(pc1_youngoldz = pc1_youngolddiff/sd_youngolddiff,
         pc2_youngoldz = pc2_youngolddiff/sd_youngolddiff,
         pc1_youngoldp = pnorm(pc1_youngoldz),
         pc2_youngoldp = pnorm(pc2_youngoldz),
         pc1_signif = ifelse(pc1_youngoldp < 0.001, "***", 
                             ifelse(pc1_youngoldp < 0.01, "**",
                                    ifelse(pc1_youngoldp < 0.05, "*",
                                           ifelse(pc1_youngoldp < 0.1, "m",
                                                  "ns")))),
         pc2_signif = ifelse(pc2_youngoldp < 0.001, "***", 
                             ifelse(pc2_youngoldp < 0.01, "**",
                                    ifelse(pc2_youngoldp < 0.05, "*",
                                           ifelse(pc2_youngoldp < 0.1, "m",
                                                  "ns"))))) %>%
  select(character, pc1_youngoldz, pc1_youngoldp, pc1_signif, pc2_youngoldz, pc2_youngoldp, pc2_signif)
d1_age

################################################### analysis & plots pt 2 #####

# -- PRINCIPAL COMPONENTS ANALYSIS B ------------------------------------------

# NOTES: 
# - good resource: http://www.colorado.edu/geography/class_homepages/geog_4023_s11/Lecture18_PCA.pdf

# --------> 11-factor (maximal) PCA (UNrotated, using principal) --------------

# extract factors
pca_B11 = principal(d3, nfactors = 11, rotate = "none"); pca_B11
# retain 3-4 components (prop var > 5-10%)
# retain 3-11 components (cumulative prop var > 70%... but < 100%?)

# extract eigenvalues
pca_B11$values # retain 3 components (eigenvalue > 1)

# scree test
qplot(y = pca_B11$values) +
  theme_bw() +
  labs(title = "Scree test for 4-factor (maximal) PCA",
       x = "Component",
       y = "Eigenvalue") +
  geom_line() # retain 3 components (left of "break")

# --------> 3-factor PCA (varimax rotation, using principal) ------------------

# extract factors
pca_B3 = principal(d3, nfactors = 3, rotate = "varimax"); pca_B3

# extract eigenvalues
pca_B3$values

# extract PCA loadings
pca_B3_pc1 = pca_B3$loadings[,1]; sort(pca_B3_pc1)
pca_B3_pc2 = pca_B3$loadings[,2]; sort(pca_B3_pc2)
pca_B3_pc3 = pca_B3$loadings[,3]; sort(pca_B3_pc3)

# --------------->-> plots ----------------------------------------------------

# 3d scatterplot of character factor loadings
with(data.frame(pca_B3$loadings[,1:3]), {
  s3d <- scatterplot3d(RC1, RC2, RC3,
                       highlight.3d = T, pch = 19,
                       type = "h",
                       main = "Character factor loadings",
                       xlab = "Rotated Component 1: 'Vulnerability'",
                       ylab = "Rotated Component 2: 'Animal Nature'",
                       zlab = "Rotated Component 3: 'Unfamiliarity'")
  # convert 3d coordinates to 2d projection
  s3d.coords <- s3d$xyz.convert(RC1, RC2, RC3)
  text(s3d.coords$x, s3d.coords$y,
       labels = row.names(data.frame(pca_B3$loadings[,1:3])),
       cex = 1, pos = 4, srt = 15)
})

# plot each pair of PCs against each other
pca_B3_factorplot12 = ggplot(data.frame(pca_B3$loadings[1:13,]), aes(x = RC1, y = RC2, label = names(d3))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings:\nRC1 vs. RC2\n",
       x = "\nRotated Component 1",
       y = "Rotated Component 2\n")
pca_B3_factorplot13 = ggplot(data.frame(pca_B3$loadings[1:13,]), aes(x = RC1, y = RC3, label = names(d3))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings:\nRC1 vs. RC3\n",
       x = "\nRotated Component 1",
       y = "Rotated Component 3\n")
pca_B3_factorplot23 = ggplot(data.frame(pca_B3$loadings[1:13,]), aes(x = RC2, y = RC3, label = names(d3))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings:\nRC2 vs. RC3\n",
       x = "\nRotated Component 2",
       y = "Rotated Component 3\n")
multiplot(pca_B3_factorplot12, pca_B3_factorplot13, pca_B3_factorplot23, cols = 3)

# 3d scatterplot of subject scores
# create dataframe
pca_B3_scores_df = data.frame(pca_B3$scores) %>%
  mutate(subid = rownames(pca_B3$scores)) %>%
  full_join(condmeans_table[,1:2]) %>%
  mutate(pcolor = ifelse(condition == "Fear", "red",
                         ifelse(condition == "Hunger", "orange",
                                ifelse(condition == "Morality", "green4",
                                       ifelse(condition == "SelfControl", "blue",
                                              NA)))))
with(pca_B3_scores_df, {
  s3d <- scatterplot3d(RC1, RC2, RC3,
                       color = pcolor, pch = 19,
                       type = "h",
                       main = "Participant factor scores by condition",
                       xlab = "Rotated Component 1: 'Vulnerability'",
                       ylab = "Rotated Component 2: 'Animal Nature'",
                       zlab = "Rotated Component 3: 'Unfamiliarity'")
  # convert 3d coordinates to 2d projection
  s3d.coords <- s3d$xyz.convert(RC1, RC2, RC3)
  legend("topleft", inset = 0,
         bty = "n", cex = .7,
         title = "Condition", 
         c("Fear", "Hunger", "Morality", "SelfControl"), 
         fill=c("red", "orange", "green4", "blue"))
})

# plot each pair of PCs against each other
pca_B3_subjectplot12 = ggplot(pca_B3_scores_df, aes(x = RC1, y = RC2, colour = condition)) +
  geom_point() +
  theme_bw() +
  labs(title = "Factor scores:\nRC1 vs. RC2\n",
       x = "\nRotated Component 1: 'Vulnerability'",
       y = "Rotated Component 2: 'Animal Nature'\n") +
  theme(legend.position = "bottom")
pca_B3_subjectplot13 = ggplot(pca_B3_scores_df, aes(x = RC1, y = RC3, colour = condition)) +
  geom_point() +
  theme_bw() +
  labs(title = "Factor scores:\nRC1 vs. RC3\n",
       x = "\nRotated Component 1: 'Vulnerability'",
       y = "Rotated Component 3: 'Unfamiliarity'\n") +
  theme(legend.position = "bottom")
pca_B3_subjectplot23 = ggplot(pca_B3_scores_df, aes(x = RC2, y = RC3, colour = condition)) +
  geom_point() +  
  theme_bw() +
  labs(title = "Factor scores:\nRC2 vs. RC3\n",
       x = "\nRotated Component 2: 'Animal Nature'",
       y = "Rotated Component 3: 'Unfamiliarity'\n") +
  theme(legend.position = "bottom")
multiplot(pca_B3_subjectplot12, pca_B3_subjectplot13, pca_B3_subjectplot23, cols = 3)

# --------> 2-factor PCA (varimax rotation, using principal) ------------------
# extract factors
pca_B2 = principal(d3, nfactors = 2, rotate = "varimax"); pca_B2

# extract PCA loadings
pca_B2_pc1 = pca_B2$loadings[,1]; sort(pca_B2_pc1)
pca_B2_pc2 = pca_B2$loadings[,2]; sort(pca_B2_pc2)

################################################### analysis & plots pt 3 #####

# -- MULTIDIMENSIONAL SCALING ANALYSIS A --------------------------------------

# --------> data formatting ---------------------------------------------------
dissim = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim <- dd %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim <- dissim %>%
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
dissim <- dissim %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim <- dissim %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim = dissim[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim = dissim[-1]
rownames(dissim) = names
colnames(dissim) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim[j,i] = dissim[i,j]
  }
}

dissim = as.dist(dissim)

# --------> non-metric (ordinal) MDS ------------------------------------------
# NOTE: could also explore fitting with more than 2 dimensions...

# do MDS
mds_Aordinal = mds(dissim, ndim = 2, type = "ordinal")
summary(mds_Aordinal)
mds_Aordinal

# --------------->-> plots ----------------------------------------------------

# plot dimension space
plot(mds_Aordinal,
     plot.type = "confplot",
     xlim = c(-1, 1),
     ylim = c(-1, 1),     
     main = "MDS solution: All conditions")

# add axes?
plot(mds_Aordinal,
     plot.type = "confplot",
     xlim = c(-1, 1),
     ylim = c(-1, 1),     
     main = "MDS solution: All conditions")
abline(0, 1, lty = 2)
abline(0, -1, lty = 2)
arrows(-.55, .65, -.45, .75, col = "green4")
text(-.3, .85, "more experience", col = "green4")
arrows(-.55, -.65, -.45, -.75, col = "red")
text(-.3, -.85, "more agency", col = "red")

# plot space and stress (bigger bubble = better fit)
plot(mds_Aordinal, plot.type = "bubbleplot",
     xlim = c(-1, 1),
     ylim = c(-1, 1),     
     main = "MDS bubble plot: All conditions")

# plot stress (higher = worse fit)
plot(mds_Aordinal, plot.type = "stressplot",
     main = "MDS stress: All conditions")

# Shepard plot
plot(mds_Aordinal, plot.type = "Shepard",
     main = "MDS Shepard plot: All conditions")

# plot residuals
plot(mds_Aordinal, plot.type = "resplot",
     main = "MDS residuals: All conditions")

################################################### analysis & plots pt 4 #####

# -- MULTIDIMENSIONAL SCALING ANALYSIS B --------------------------------------

# --------> data formatting ---------------------------------------------------

# --------------->-> condition: FEAR ------------------------------------------

dissim_fear = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_fear <- dd %>%
  filter(condition == "Fear") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_fear$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_fear <- dissim_fear %>%
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

# make upper matrix of dissim_fearilarity values
dissim_fear <- dissim_fear %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_fear <- dissim_fear %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_fear = dissim_fear[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_fear = dissim_fear[-1]
rownames(dissim_fear) = names
colnames(dissim_fear) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_fear[j,i] = dissim_fear[i,j]
  }
}

dissim_fear = as.dist(dissim_fear)

# --------------->-> condition: HUNGER ----------------------------------------

dissim_hunger = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_hunger <- dd %>%
  filter(condition == "Hunger") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_hunger$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_hunger <- dissim_hunger %>%
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

# make upper matrix of dissim_hungerilarity values
dissim_hunger <- dissim_hunger %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_hunger <- dissim_hunger %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_hunger = dissim_hunger[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_hunger = dissim_hunger[-1]
rownames(dissim_hunger) = names
colnames(dissim_hunger) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_hunger[j,i] = dissim_hunger[i,j]
  }
}

dissim_hunger = as.dist(dissim_hunger)

# --------------->-> condition: MORALITY --------------------------------------

dissim_morality = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_morality <- dd %>%
  filter(condition == "Morality") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_morality$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_morality <- dissim_morality %>%
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

# make upper matrix of dissim_moralityilarity values
dissim_morality <- dissim_morality %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_morality <- dissim_morality %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_morality = dissim_morality[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_morality = dissim_morality[-1]
rownames(dissim_morality) = names
colnames(dissim_morality) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_morality[j,i] = dissim_morality[i,j]
  }
}

dissim_morality = as.dist(dissim_morality)

# --------------->-> condition: SELF-CONTROL ----------------------------------

dissim_selfcontrol = NULL

# make alphabetized list of characters, cycle through to fill in alphabetized pairs
dissim_selfcontrol <- dd %>%
  filter(condition == "SelfControl") %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(dissim_selfcontrol$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  dissim_selfcontrol <- dissim_selfcontrol %>%
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

# make upper matrix of dissim_selfcontrolilarity values
dissim_selfcontrol <- dissim_selfcontrol %>%
  select(condition, subid, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
dissim_selfcontrol <- dissim_selfcontrol %>%
  mutate(charlie_dog = NA,
         character1 = as.character(character1)) %>%
  rbind(c("you", rep(NA, 13))) %>%
  mutate(character1 = factor(character1))

# reorder columns
dissim_selfcontrol = dissim_selfcontrol[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
names = 
  ifelse(names == "charlie_dog", "dog",
         ifelse(names == "delores_gleitman_deceased", "dead woman",
                ifelse(names == "gerald_schiff_pvs", "PVS man",
                       ifelse(names == "green_frog", "frog",
                              ifelse(names == "samantha_hill_girl", "girl",
                                     ifelse(names == "kismet_robot", "robot",
                                            ifelse(names == "nicholas_gannon_baby", "baby",
                                                   ifelse(names == "sharon_harvey_woman", "woman",
                                                          ifelse(names == "toby_chimp", "chimp",
                                                                 ifelse(names == "todd_billingsley_man", "man",
                                                                        as.character(names)))))))))))

dissim_selfcontrol = dissim_selfcontrol[-1]
rownames(dissim_selfcontrol) = names
colnames(dissim_selfcontrol) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    dissim_selfcontrol[j,i] = dissim_selfcontrol[i,j]
  }
}

dissim_selfcontrol = as.dist(dissim_selfcontrol)

# --------> non-metric (ordinal) MDS ------------------------------------------
# NOTE: could also explore fitting with more than 2 dimensions...

# do MDS: FEAR
mds_fear_Aordinal = mds(dissim_fear, ndim = 2, type = "ordinal")
summary(mds_fear_Aordinal)
mds_fear_Aordinal

# do MDS: HUNGER
mds_hunger_Aordinal = mds(dissim_hunger, ndim = 2, type = "ordinal")
summary(mds_hunger_Aordinal)
mds_hunger_Aordinal

# do MDS: MORALITY
mds_morality_Aordinal = mds(dissim_morality, ndim = 2, type = "ordinal")
summary(mds_morality_Aordinal)
mds_morality_Aordinal

# do MDS: SELF-CONTROL
mds_selfcontrol_Aordinal = mds(dissim_selfcontrol, ndim = 2, type = "ordinal")
summary(mds_selfcontrol_Aordinal)
mds_selfcontrol_Aordinal

# --------------->-> plots ----------------------------------------------------

# plot everything in 2x2 grid
par(mfrow = c(2,2))

# plot dimension space
plot(mds_fear_Aordinal, 
     plot.type = "confplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS solution: FEAR")
plot(mds_hunger_Aordinal, 
     plot.type = "confplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS solution: HUNGER")
plot(mds_morality_Aordinal, 
     plot.type = "confplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS solution: MORALITY")
plot(mds_selfcontrol_Aordinal, 
     plot.type = "confplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS solution: SELF-CONTROL")

# plot space and stress (bigger bubble = better fit)
plot(mds_fear_Aordinal, 
     plot.type = "bubbleplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),
     main = "MDS bubble plot: FEAR")
plot(mds_hunger_Aordinal, 
     plot.type = "bubbleplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),     
     main = "MDS bubble plot: HUNGER")
plot(mds_morality_Aordinal, 
     plot.type = "bubbleplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),     
     main = "MDS bubble plot: MORALITY")
plot(mds_selfcontrol_Aordinal, 
     plot.type = "bubbleplot", 
     xlim = c(-1, 1),
     ylim = c(-1, 1),     
     main = "MDS bubble plot: SELF-CONTROL")

# plot stress (higher = worse fit)
plot(mds_fear_Aordinal, 
     plot.type = "stressplot", 
     main = "MDS stress: FEAR")
plot(mds_hunger_Aordinal, 
     plot.type = "stressplot", 
     main = "MDS stress: HUNGER")
plot(mds_morality_Aordinal, 
     plot.type = "stressplot", 
     main = "MDS stress: MORALITY")
plot(mds_selfcontrol_Aordinal, 
     plot.type = "stressplot", 
     main = "MDS stress: SELF-CONTROL")

# Shepard plot
plot(mds_fear_Aordinal, 
     plot.type = "Shepard", 
     main = "MDS Shepard plot: FEAR")
plot(mds_hunger_Aordinal, 
     plot.type = "Shepard", 
     main = "MDS Shepard plot: HUNGER")
plot(mds_morality_Aordinal, 
     plot.type = "Shepard", 
     main = "MDS Shepard plot: MORALITY")
plot(mds_selfcontrol_Aordinal, 
     plot.type = "Shepard", 
     main = "MDS Shepard plot: SELF-CONTROL")

# plot residuals
plot(mds_fear_Aordinal, 
     plot.type = "resplot", 
     main = "MDS residuals: FEAR")
plot(mds_hunger_Aordinal, 
     plot.type = "resplot", 
     main = "MDS residuals: HUNGER")
plot(mds_morality_Aordinal, 
     plot.type = "resplot", 
     main = "MDS residuals: MORALITY")
plot(mds_selfcontrol_Aordinal, 
     plot.type = "resplot", 
     main = "MDS residuals: SELF-CONTROL")

# stop plotting in 2x2 grid
par(mfrow = c(1,1))
