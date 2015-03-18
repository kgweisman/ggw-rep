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

########################################################### supplementals #####

# --- MULTIDIMENSIONAL SCALING ANALYSES: INDIVIDUAL DIFFERENCES ---------------

# --------> MDS C: all conditions (indscal) -----------------------------------

# --------------->-> data formatting ------------------------------------------

# construct dissimilarity matrices for each participant
dissimList = list(NULL)

for(k in 1:length(levels(dd$subid))) {
  subid_temp = levels(dd$subid)[k]
  dissim_temp = NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  dissim_temp <- dd %>%
    filter(subid == subid_temp) %>%
    mutate(character1 = array(),
           character2 = array())
  
  charsort = sort(levels(dissim_temp$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    dissim_temp <- dissim_temp %>%
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
  dissim_temp <- dissim_temp %>%
    select(condition, subid, character1, character2, responseNum) %>%
    group_by(character1, character2) %>%
    mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
    summarise(mean = mean(dist, na.rm = TRUE)) %>%
    spread(character2, mean)
  
  # add in NA column for charlie_dog, NA row for you
  dissim_temp <- dissim_temp %>%
    mutate(charlie_dog = NA,
           character1 = as.character(character1)) %>%
    rbind(c("you", rep(NA, 13))) %>%
    mutate(character1 = factor(character1))
  
  # reorder columns
  dissim_temp = dissim_temp[, c(1, 14, 2:13)]
  
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
  
  dissim_temp = dissim_temp[-1]
  rownames(dissim_temp) = names
  colnames(dissim_temp) = names
  
  # fill in lower triangle matrix
  for(i in 1:12) {
    for(j in (i+1):13) {
      dissim_temp[j,i] = dissim_temp[i,j]
    }
  }
  
  dissim_temp = as.dist(dissim_temp)
  
  # add to dissimList
  dissimList[[k]] = dissim_temp
}

# --------------->-> metric (ratio) MDS ---------------------------------------

# perform 3-way MDS (ratio)
mds_Cratio = indscal(dissimList, type = "ratio", verbose = T)
summary(mds_Cratio)

# plot space
plot(mds_Cratio, plot.type = "confplot")

# plot space and stress (bigger bubble = better fit)
plot(mds_Cratio, plot.type = "bubbleplot")

# plot stress (higher = worse fit)
plot(mds_Cratio, plot.type = "stressplot")

# plot residuals
plot(mds_Cratio, plot.type = "Shepard")
plot(mds_Cratio, plot.type = "resplot")

# --------------->-> non-metric (ordinal) MDS ---------------------------------

# perform 3-way MDS (ordinal)
mds_Cordinal = indscal(dissimList, type = "ordinal", verbose = T)
summary(mds_Cordinal)
mds_Cordinal

# plot space
plot(mds_Cordinal, plot.type = "confplot",
     main = "Character dimension scores:\nAll conditions")

# # plot space and stress (bigger bubble = better fit)
# plot(mds_Cordinal, plot.type = "bubbleplot")
# 
# # plot stress (higher = worse fit)
# plot(mds_Cordinal, plot.type = "stressplot")
# 
# # plot residuals
# plot(mds_Cordinal, plot.type = "Shepard")
# plot(mds_Cordinal, plot.type = "resplot")

# --------> MDS D: by condition (indscal) -------------------------------------

# --------------->-> condition: FEAR ------------------------------------------

# ---------------------->->-> data formatting ---------------------------------

# filter by condition
dd_fear = dd %>% filter(condition == "Fear") %>% mutate(subid = factor(subid))

# construct dissimilarity matrices for each participant
dissimList_fear = list(NULL)

for(k in 1:length(levels(dd_fear$subid))) {
  subid_temp = levels(dd_fear$subid)[k]
  dissim_temp = NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  dissim_temp <- dd_fear %>%
    filter(subid == subid_temp) %>%
    mutate(character1 = array(),
           character2 = array())
  
  charsort = sort(levels(dissim_temp$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    dissim_temp <- dissim_temp %>%
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
  dissim_temp <- dissim_temp %>%
    select(condition, subid, character1, character2, responseNum) %>%
    group_by(character1, character2) %>%
    mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
    summarise(mean = mean(dist, na.rm = TRUE)) %>%
    spread(character2, mean)
  
  # add_fear in NA column for charlie_dog, NA row for you
  dissim_temp <- dissim_temp %>%
    mutate(charlie_dog = NA,
           character1 = as.character(character1)) %>%
    rbind(c("you", rep(NA, 13))) %>%
    mutate(character1 = factor(character1))
  
  # reorder columns
  dissim_temp = dissim_temp[, c(1, 14, 2:13)]
  
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
  
  dissim_temp = dissim_temp[-1]
  rownames(dissim_temp) = names
  colnames(dissim_temp) = names
  
  # fill in lower triangle matrix
  for(i in 1:12) {
    for(j in (i+1):13) {
      dissim_temp[j,i] = dissim_temp[i,j]
    }
  }
  
  dissim_temp = as.dist(dissim_temp)
  
  # add_fear to dissimList_fear
  dissimList_fear[[k]] = dissim_temp
}

# ---------------------->->-> metric (ratio) MDS ------------------------------

# perform 3-way MDS (ordinal)
mds_fear_Dratio = indscal(dissimList_fear, type = "ratio", verbose = T)
summary(mds_fear_Dratio)
mds_fear_Dratio

# ----------------------------->->->-> plots ----------------------------------

# plot space
plot(mds_fear_Dratio, plot.type = "confplot",
     main = "Character dimension scores:\nFEAR")

# # plot space and stress (bigger buBale = better fit)
# plot(mds_fear_Dratio, plot.type = "buBaleplot", sub = "Condition: FEAR")
# 
# # plot stress (higher = worse fit)
# plot(mds_fear_Dratio, plot.type = "stressplot", sub = "Condition: FEAR")
# 
# # plot residuals
# plot(mds_fear_Dratio, plot.type = "Shepard", sub = "Condition: FEAR")
# plot(mds_fear_Dratio, plot.type = "resplot", sub = "Condition: FEAR")

# ---------------------->->-> non-metric (ordinal) MDS ------------------------

# perform 3-way MDS (ordinal)
mds_fear_Dordinal = indscal(dissimList_fear, type = "ordinal", verbose = T)
summary(mds_fear_Dordinal)
mds_fear_Dordinal

# ----------------------------->->->-> plots ----------------------------------

# plot space
plot(mds_fear_Dordinal, plot.type = "confplot",
     main = "Character dimension scores:\nFEAR")

# # plot space and stress (bigger bubble = better fit)
# plot(mds_fear_Dordinal, plot.type = "bubbleplot", sub = "Condition: FEAR")
# 
# # plot stress (higher = worse fit)
# plot(mds_fear_Dordinal, plot.type = "stressplot", sub = "Condition: FEAR")
# 
# # plot residuals
# plot(mds_fear_Dordinal, plot.type = "Shepard", sub = "Condition: FEAR")

# --------------->-> condition: HUNGER ----------------------------------------

# ---------------------->->-> data formatting ---------------------------------

# filter by condition
dd_hunger = dd %>% filter(condition == "Hunger") %>% mutate(subid = factor(subid))

# construct dissimilarity matrices for each participant
dissimList_hunger = list(NULL)

for(k in 1:length(levels(dd_hunger$subid))) {
  subid_temp = levels(dd_hunger$subid)[k]
  dissim_temp = NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  dissim_temp <- dd_hunger %>%
    filter(subid == subid_temp) %>%
    mutate(character1 = array(),
           character2 = array())
  
  charsort = sort(levels(dissim_temp$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    dissim_temp <- dissim_temp %>%
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
  dissim_temp <- dissim_temp %>%
    select(condition, subid, character1, character2, responseNum) %>%
    group_by(character1, character2) %>%
    mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
    summarise(mean = mean(dist, na.rm = TRUE)) %>%
    spread(character2, mean)
  
  # add_hunger in NA column for charlie_dog, NA row for you
  dissim_temp <- dissim_temp %>%
    mutate(charlie_dog = NA,
           character1 = as.character(character1)) %>%
    rbind(c("you", rep(NA, 13))) %>%
    mutate(character1 = factor(character1))
  
  # reorder columns
  dissim_temp = dissim_temp[, c(1, 14, 2:13)]
  
  # rename rows and columns
  names = sort(charsort, decreasing = FALSE)
  names = 
    ifelse(names == "charlie_dog", "dog",
           ifelse(names == "delores_gleitman_deceased", "dead woman",
                  ifelse(names == "gerald_schiff_pvs", "PVS man",
                         ifelse(names == "green_frog", "frog",
                                ifelse(names == "samantha_hill_girl", "girl",
                                       ifelse(names == "kismet_robot", "robot",
                                              ifelse(names == "nicholas_gannon_Dratioby", "baby",
                                                     ifelse(names == "sharon_harvey_woman", "woman",
                                                            ifelse(names == "toby_chimp", "chimp",
                                                                   ifelse(names == "todd_billingsley_man", "man",
                                                                          as.character(names)))))))))))
  
  dissim_temp = dissim_temp[-1]
  rownames(dissim_temp) = names
  colnames(dissim_temp) = names
  
  # fill in lower triangle matrix
  for(i in 1:12) {
    for(j in (i+1):13) {
      dissim_temp[j,i] = dissim_temp[i,j]
    }
  }
  
  dissim_temp = as.dist(dissim_temp)
  
  # add_hunger to dissimList_hunger
  dissimList_hunger[[k]] = dissim_temp
}

# ---------------------->->-> metric (ratio) MDS ------------------------------

# perform 3-way MDS (ordinal)
mds_hunger_Dratio = indscal(dissimList_hunger, type = "ratio", verbose = T)
summary(mds_hunger_Dratio)
mds_hunger_Dratio

# ----------------------------->->->-> plots ----------------------------------

# plot space
plot(mds_hunger_Dratio, plot.type = "confplot",
     main = "Character dimension scores:\nHUNGER")

# # plot space and stress (bigger buBale = better fit)
# plot(mds_hunger_Dratio, plot.type = "buBaleplot", sub = "Condition: HUNGER")
# 
# # plot stress (higher = worse fit)
# plot(mds_hunger_Dratio, plot.type = "stressplot", sub = "Condition: HUNGER")
# 
# # plot residuals
# plot(mds_hunger_Dratio, plot.type = "Shepard", sub = "Condition: HUNGER")
# plot(mds_hunger_Dratio, plot.type = "resplot", sub = "Condition: HUNGER")

# ---------------------->->-> non-metric (ordinal) MDS ------------------------

# perform 3-way MDS (ordinal)
mds_hunger_Dordinal = indscal(dissimList_hunger, type = "ordinal", verbose = T)
summary(mds_hunger_Dordinal)
mds_hunger_Dordinal

# ----------------------------->->->-> plots ----------------------------------

# plot space
plot(mds_hunger_Dordinal, plot.type = "confplot",
     main = "Character dimension scores:\nHUNGER")

# # plot space and stress (bigger bubble = better fit)
# plot(mds_hunger_Dordinal, plot.type = "bubbleplot", sub = "Condition: HUNGER")
# 
# # plot stress (higher = worse fit)
# plot(mds_hunger_Dordinal, plot.type = "stressplot", sub = "Condition: HUNGER")
# 
# # plot residuals
# plot(mds_hunger_Dordinal, plot.type = "Shepard", sub = "Condition: HUNGER")

# --------------->-> condition: MORALITY --------------------------------------

# ---------------------->->-> data formatting ---------------------------------

# filter by condition
dd_morality = dd %>% filter(condition == "Morality") %>% mutate(subid = factor(subid))

# construct dissimilarity matrices for each participant
dissimList_morality = list(NULL)

for(k in 1:length(levels(dd_morality$subid))) {
  subid_temp = levels(dd_morality$subid)[k]
  dissim_temp = NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  dissim_temp <- dd_morality %>%
    filter(subid == subid_temp) %>%
    mutate(character1 = array(),
           character2 = array())
  
  charsort = sort(levels(dissim_temp$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    dissim_temp <- dissim_temp %>%
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
  dissim_temp <- dissim_temp %>%
    select(condition, subid, character1, character2, responseNum) %>%
    group_by(character1, character2) %>%
    mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
    summarise(mean = mean(dist, na.rm = TRUE)) %>%
    spread(character2, mean)
  
  # add_morality in NA column for charlie_dog, NA row for you
  dissim_temp <- dissim_temp %>%
    mutate(charlie_dog = NA,
           character1 = as.character(character1)) %>%
    rbind(c("you", rep(NA, 13))) %>%
    mutate(character1 = factor(character1))
  
  # reorder columns
  dissim_temp = dissim_temp[, c(1, 14, 2:13)]
  
  # rename rows and columns
  names = sort(charsort, decreasing = FALSE)
  names = 
    ifelse(names == "charlie_dog", "dog",
           ifelse(names == "delores_gleitman_deceased", "dead woman",
                  ifelse(names == "gerald_schiff_pvs", "PVS man",
                         ifelse(names == "green_frog", "frog",
                                ifelse(names == "samantha_hill_girl", "girl",
                                       ifelse(names == "kismet_robot", "robot",
                                              ifelse(names == "nicholas_gannon_Dratioby", "baby",
                                                     ifelse(names == "sharon_harvey_woman", "woman",
                                                            ifelse(names == "toby_chimp", "chimp",
                                                                   ifelse(names == "todd_billingsley_man", "man",
                                                                          as.character(names)))))))))))
  
  dissim_temp = dissim_temp[-1]
  rownames(dissim_temp) = names
  colnames(dissim_temp) = names
  
  # fill in lower triangle matrix
  for(i in 1:12) {
    for(j in (i+1):13) {
      dissim_temp[j,i] = dissim_temp[i,j]
    }
  }
  
  dissim_temp = as.dist(dissim_temp)
  
  # add_morality to dissimList_morality
  dissimList_morality[[k]] = dissim_temp
}

# ---------------------->->-> metric (ratio) MDS ------------------------------

# perform 3-way MDS (ordinal)
mds_morality_Dratio = indscal(dissimList_morality, type = "ratio", verbose = T)
summary(mds_morality_Dratio)
mds_morality_Dratio

# ----------------------------->->->-> plots ----------------------------------

# plot space
plot(mds_morality_Dratio, plot.type = "confplot",
     main = "Character dimension scores:\nMORALITY")

# # plot space and stress (bigger buBale = better fit)
# plot(mds_morality_Dratio, plot.type = "buBaleplot", sub = "Condition: MORALITY")
# 
# # plot stress (higher = worse fit)
# plot(mds_morality_Dratio, plot.type = "stressplot", sub = "Condition: MORALITY")
# 
# # plot residuals
# plot(mds_morality_Dratio, plot.type = "Shepard", sub = "Condition: MORALITY")
# plot(mds_morality_Dratio, plot.type = "resplot", sub = "Condition: MORALITY")

# ---------------------->->-> non-metric (ordinal) MDS ------------------------

# perform 3-way MDS (ordinal)
mds_morality_Dordinal = indscal(dissimList_morality, type = "ordinal", verbose = T)
summary(mds_morality_Dordinal)
mds_morality_Dordinal

# ----------------------------->->->-> plots ----------------------------------

# plot space
plot(mds_morality_Dordinal, plot.type = "confplot",
     main = "Character dimension scores:\nMORALITY")

# # plot space and stress (bigger bubble = better fit)
# plot(mds_morality_Dordinal, plot.type = "bubbleplot", sub = "Condition: MORALITY")
# 
# # plot stress (higher = worse fit)
# plot(mds_morality_Dordinal, plot.type = "stressplot", sub = "Condition: MORALITY")
# 
# # plot residuals
# plot(mds_morality_Dordinal, plot.type = "Shepard", sub = "Condition: MORALITY")

# --------------->-> condition: SELF-CONTROL ----------------------------------

# ---------------------->->-> data formatting ---------------------------------

# filter by condition
dd_selfcontrol = dd %>% filter(condition == "SelfControl") %>% mutate(subid = factor(subid))

# construct dissimilarity matrices for each participant
dissimList_selfcontrol = list(NULL)

for(k in 1:length(levels(dd_selfcontrol$subid))) {
  subid_temp = levels(dd_selfcontrol$subid)[k]
  dissim_temp = NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  dissim_temp <- dd_selfcontrol %>%
    filter(subid == subid_temp) %>%
    mutate(character1 = array(),
           character2 = array())
  
  charsort = sort(levels(dissim_temp$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    dissim_temp <- dissim_temp %>%
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
  dissim_temp <- dissim_temp %>%
    select(condition, subid, character1, character2, responseNum) %>%
    group_by(character1, character2) %>%
    mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
    summarise(mean = mean(dist, na.rm = TRUE)) %>%
    spread(character2, mean)
  
  # add_selfcontrol in NA column for charlie_dog, NA row for you
  dissim_temp <- dissim_temp %>%
    mutate(charlie_dog = NA,
           character1 = as.character(character1)) %>%
    rbind(c("you", rep(NA, 13))) %>%
    mutate(character1 = factor(character1))
  
  # reorder columns
  dissim_temp = dissim_temp[, c(1, 14, 2:13)]
  
  # rename rows and columns
  names = sort(charsort, decreasing = FALSE)
  names = 
    ifelse(names == "charlie_dog", "dog",
           ifelse(names == "delores_gleitman_deceased", "dead woman",
                  ifelse(names == "gerald_schiff_pvs", "PVS man",
                         ifelse(names == "green_frog", "frog",
                                ifelse(names == "samantha_hill_girl", "girl",
                                       ifelse(names == "kismet_robot", "robot",
                                              ifelse(names == "nicholas_gannon_Dratioby", "baby",
                                                     ifelse(names == "sharon_harvey_woman", "woman",
                                                            ifelse(names == "toby_chimp", "chimp",
                                                                   ifelse(names == "todd_billingsley_man", "man",
                                                                          as.character(names)))))))))))
  
  dissim_temp = dissim_temp[-1]
  rownames(dissim_temp) = names
  colnames(dissim_temp) = names
  
  # fill in lower triangle matrix
  for(i in 1:12) {
    for(j in (i+1):13) {
      dissim_temp[j,i] = dissim_temp[i,j]
    }
  }
  
  dissim_temp = as.dist(dissim_temp)
  
  # add_selfcontrol to dissimList_selfcontrol
  dissimList_selfcontrol[[k]] = dissim_temp
}

# ---------------------->->-> metric (ratio) MDS ------------------------------

# perform 3-way MDS (ordinal)
mds_selfcontrol_Dratio = indscal(dissimList_selfcontrol, type = "ratio", verbose = T)
summary(mds_selfcontrol_Dratio)
mds_selfcontrol_Dratio

# ----------------------------->->->-> plots ----------------------------------

# perform 3-way MDS (ordinal)
mds_selfcontrol_Dratio = indscal(dissimList_selfcontrol, type = "ratio", verbose = T)
summary(mds_selfcontrol_Dratio)
mds_selfcontrol_Dratio

# plot space
plot(mds_selfcontrol_Dratio, plot.type = "confplot",
     main = "Character dimension scores:\nSELF-CONTROL")

# # plot space and stress (bigger buBale = better fit)
# plot(mds_selfcontrol_Dratio, plot.type = "buBaleplot", sub = "Condition: SELF-CONTROL")
# 
# # plot stress (higher = worse fit)
# plot(mds_selfcontrol_Dratio, plot.type = "stressplot", sub = "Condition: SELF-CONTROL")
# 
# # plot residuals
# plot(mds_selfcontrol_Dratio, plot.type = "Shepard", sub = "Condition: SELF-CONTROL")
# plot(mds_selfcontrol_Dratio, plot.type = "resplot", sub = "Condition: SELF-CONTROL")

# ---------------------->->-> non-metric (ordinal) MDS ------------------------

# perform 3-way MDS (ordinal)
mds_selfcontrol_Dordinal = indscal(dissimList_selfcontrol, type = "ordinal", verbose = T)
summary(mds_selfcontrol_Dordinal)
mds_selfcontrol_Dordinal

# ----------------------------->->->-> plots ----------------------------------

# plot space
plot(mds_selfcontrol_Dordinal, plot.type = "confplot",
     main = "Character dimension scores:\nSELF-CONTROL")

# # plot space and stress (bigger bubble = better fit)
# plot(mds_selfcontrol_Dordinal, plot.type = "bubbleplot", sub = "Condition: SELF-CONTROL")
# 
# # plot stress (higher = worse fit)
# plot(mds_selfcontrol_Dordinal, plot.type = "stressplot", sub = "Condition: SELF-CONTROL")
# 
# # plot residuals
# plot(mds_selfcontrol_Dordinal, plot.type = "Shepard", sub = "Condition: SELF-CONTROL")

# --- MAXIMUM LIKELIHOOD FACTOR ANALYSIS A ------------------------------------
# Roughly equivalent to pca_A?
# Could also do the parallel version of pca_B

# Factor analysis
fa1 = factanal(d1, 
               factors = 1, 
               rotation = "varimax", 
               na.action = na.omit, 
               scores =  'regression', 
               cutoff = .4)
print(fa1)

# --- HIERARCHICAL CLUSTER ANALYSIS A -----------------------------------------
# Roughly equivalent to pca_A
# Could also do the parallel version of pca_B

# Construct dissimilarity matrix
d2 = as.dist((1-cor(d1))/2) # NEED TO CHECK ON WHY WE DIVIDE CORRELATIONS BY 2

# Conduct hierarchical cluster analysis
hca = hclust(d2); hca

# Plot dendogram
par(mfrow=c(1,2))
rs1=hclust(d2)
rs1$merge
plot(rs1$height)
plot(rs1)