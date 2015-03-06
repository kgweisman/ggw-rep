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


# --- PRINCIPAL COMPONENTS ANALYSIS #1: ORIGINAL GGW2007 (?) ------------------

d1 = charmeans_table[2:6]

# Extract 2 PCs (unrotated)
pca1 = principal(d1, nfactors = 2, rotate = "none"); pca1

# Extract 2 PCs (rotated)
pca2 = principal(d1, nfactors = 2, rotate = "varimax"); pca2

# Plot PCs against each other for both solutions
par(mfrow=c(1,2))
variables = names(d1)
plot(pca1$loadings, type='n')
text(pca1$loadings, labels=variables, cex=.9)
plot(pca2$loadings, type='n')
text(pca2$loadings, labels=variables, cex=.9)

# Extract PCA loadings for PC1 (unrotated)
pc1 = pca1$loadings[,1]

# Extract PCA loadings for PC2 (unrotated)
pc2 = pca1$loadings[,1]

# Set min and max 
m1.pca = min(c(pc1, pc2))
m2.pca = max(c(pc1, pc2))

# Plot participants by principle components (unrotated)
plot.pca = plot(pc1, pc2, xlim = c(m1.pca, m2.pca)); plot.pca
plot.pca = plot(pc1, pc2, xlim = c(m1.pca, m2.pca)); plot.pca

# --- MAXIMUM LIKELIHOOD FACTOR ANALYSIS #1 -----------------------------------

# Factor analysis
fa1 = factanal(d1, 
               factors = 2, 
               rotation = "varimax", 
               na.action = na.omit, 
               scores =  'regression', 
               cutoff = .4)
print(fa1)


# --- HIERARCHICAL CLUSTER ANALYSIS -------------------------------------------

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
