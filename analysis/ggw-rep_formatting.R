# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(stats)

# clear environment
rm(list=ls())

# set working directory for india
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/turk/pilot-b_01/")

# mike's json for-loop
files <- dir("production-results/")

d.raw <- data.frame()

for(i in 1:length(files)) {
  # gather files
  f = files[i]
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  
  # store relevant variables in dataframe 
  id <- data.frame(
    matrix(
    data = c("subid", "condition", "age", "gender", "ethnicity", "education", 
               "religionChild", "religionNow", "job", "maritalStatus", 
               "children", "country", "englishNative", "politicalIdeology", 
               "studyMoralPhil", "vegetarian", "beliefGod", "beliefAfterlife", 
               "beliefTradition", "beliefRules", "beliefLeader", "comments", 
               "trialNum", "leftCharacter", "rightCharacter", "response", "rt"),
    nrow = 78, ncol = 0))

  # subject-level data: identity
  id$subid = paste0("S",i)
  id$condition = jd$answers$data$newData$condition
  
  # subject-level data: demographics
  id$age = jd$answers$data$newData$age
  id$gender = jd$answers$data$newData$gender
  id$ethnicity = jd$answers$data$newData$ethnicity
  id$education = jd$answers$data$newData$education
  id$religionChild = jd$answers$data$newData$religionChild
  id$religionNow = jd$answers$data$newData$religionNow
  id$job = jd$answers$data$newData$job
  id$maritalStatus = jd$answers$data$newData$maritalStatus
  id$children = jd$answers$data$newData$children
  id$country = jd$answers$data$newData$country
  id$englishNative = jd$answers$data$newData$englishNative
  id$politicalIdeology = jd$answers$data$newData$politicalIdeology
  id$studyMoralPhil = jd$answers$data$newData$studyMoralPhil
  id$vegetarian = jd$answers$data$newData$vegetarian
  id$beliefGod = jd$answers$data$newData$beliefGod
  id$beliefAfterlife = jd$answers$data$newData$beliefAfterlife
  id$beliefTradition = jd$answers$data$newData$beliefTradition
  id$beliefRules = jd$answers$data$newData$beliefRules
  id$beliefLeader = jd$answers$data$newData$beliefLeader
  id$comments = jd$answers$data$newData$comments
  
  # trial-level data:                    
  id$trialNum = jd$answers$data$newData$trialData$trialNum
  id$leftCharacter = jd$answers$data$newData$trialData$leftCharacter
  id$rightCharacter = jd$answers$data$newData$trialData$rightCharacter
  id$response = jd$answers$data$newData$trialData$response
  id$rt = jd$answers$data$newData$trialData$rt

  # bind into same dataframe
  d.raw <- bind_rows(d.raw, id)
}

glimpse(d.raw)

# clean up variables
d_tidy = d.raw %>%
  mutate(subid = factor(subid),
         condition = factor(condition),
         age = as.numeric(age),
         gender = factor(gender),
         ethnicity = factor(ethnicity), # redo for multiple selected
         education = factor(education),
         religionChild = factor(religionChild), # redo for multiple selected
         religionNow = factor(religionNow), # redo for multiple selected
         children = factor(children),
         englishNative = factor(englishNative),
         politicalIdeology = factor(politicalIdeology),         
         studyMoralPhil = factor(studyMoralPhil),         
         vegetarian = factor(vegetarian),         
         beliefGod = factor(beliefGod),         
         beliefAfterlife = factor(beliefAfterlife),         
         beliefTradition = factor(beliefTradition),         
         beliefRules = factor(beliefRules),         
         beliefLeader = factor(beliefLeader),         
         leftCharacter = factor(leftCharacter),         
         rightCharacter = factor(rightCharacter),         
         response = factor(response),
         responseNum = 
           ifelse(response == "much more left", -2,
                  ifelse(response == "slightly more left", -1,
                         ifelse(response == "both equally", 0,
                                ifelse(response == "slightly more right", 1,
                                       ifelse(response == "much more right", 2, NA)))))
         )

glimpse(d_tidy)

# write to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/pilot-b_01_data_anonymized.csv")

d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/pilot-b_01_data_anonymized.csv")[-1] # get rid of column of obs numbers
