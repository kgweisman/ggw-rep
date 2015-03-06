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



