# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(stats)

# clear environment
rm(list=ls())

# read in anonymized data
d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-rep/ggw-rep/data/pilot-b_01_charmeans.csv")[-1] # get rid of column of obs numbers

glimpse(d)

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
