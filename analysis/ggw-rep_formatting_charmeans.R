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
  
  kd <- data.frame(matrix(
    data = c("subid", "condition", "gerald_schiff_pvs", "toby_chimp", "fetus",
             "god", "delores_gleitman_deceased", "sharon_harvey_woman",
             "green_frog", "todd_billingsley_man", "charlie_dog", 
             "nicholas_gannon_baby", "samantha_hill_girl", "kismet_robot", 
             "you"),
    nrow = 12, 
    ncol = 0))

  kd$subid = paste0("S",i)
  kd$condition = jd$answers$data$newData$condition
  kd$gerald_schiff_pvs = jd$answers$data$newData$charScores$gerald_schiff_pvs
  kd$toby_chimp = jd$answers$data$newData$charScores$toby_chimp
  kd$fetus = jd$answers$data$newData$charScores$fetus
  kd$god = jd$answers$data$newData$charScores$god
  kd$delores_gleitman_deceased = jd$answers$data$newData$charScores$delores_gleitman_deceased
  kd$sharon_harvey_woman = jd$answers$data$newData$charScores$sharon_harvey_woman
  kd$green_frog = jd$answers$data$newData$charScores$green_frog
  kd$todd_billingsley_man = jd$answers$data$newData$charScores$todd_billingsley_man
  kd$charlie_dog = jd$answers$data$newData$charScores$charlie_dog
  kd$nicholas_gannon_baby = jd$answers$data$newData$charScores$nicholas_gannon_baby
  kd$samantha_hill_girl = jd$answers$data$newData$charScores$samantha_hill_girl
  kd$kismet_robot = jd$answers$data$newData$charScores$kismet_robot
  kd$you = jd$answers$data$newData$charScores$you
  
  # bind into same dataframe
  d.raw <- bind_rows(d.raw, kd)
}

glimpse(d.raw)

# clean up variables
d_tidy = d.raw %>%
  mutate(subid = factor(subid),
         condition = factor(condition))

glimpse(d_tidy)

# write to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/pilot-b_01_charmeans.csv")

d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/pilot-b_01_charmeans.csv")[-1] # get rid of column of obs numbers

