
# +++++++++++++++++++++++++++++++++++
# Linear regression of Africa samples 
# +++++++++++++++++++++++++++++++++++

# Library setup ----
library(ggplot2) #For plotting the distribution of data
library(pscl) #For the zero-inflation model
library(MASS)
library(boot)
library(Hmisc)
library(corrplot)

# Data setup ----
data = read.csv("/Users/hasansulaeman/Dropbox/Africa Bd Project/Africa manuscript/data/Sonia_Hirschfeldetal_Data_for_GLM_01102018.csv")
corr = data[,28:68]

# Finding correlations to eliminate factors ----
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
factor.cor = rcorr(as.matrix(corr) , type= "pearson")
corr.table = flattenCorrMatrix(factor.cor$r, factor.cor$P)
write.csv(corr.table, "correlationtable.csv")
relevant = subset(corr.table, corr.table$cor >= 0.9)
corrplot(factor.cor$r, method="number")

# For Africa, we have a total of 27 covariates with a cutoff point of 90%/-90% in terms of correlations 
# Covariates excluded: bio5, bio6, bio8, bio9, bio10, bio11, elev, bio12, bio13, bio16, bio17, ai_yr

# Doing the linear regression for infection status ----
data = read.csv("/Users/hasansulaeman/Dropbox/Africa Bd Project/Africa manuscript/data/Sonia_Hirschfeldetal_Data_for_LR_01102018.csv")
s.data = scale(data[,28:56])
data[,28:56] = s.data
lm = lm(a, ,  , data = data)
