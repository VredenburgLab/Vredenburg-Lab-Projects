
# Calculate cred intervals for Asia samples (Rockney et al.)

# Setting up libraries and dataset ----
library(reshape2)
data = read.csv("Sulawesi Bd Survey (Rockney et al.)/Review table.csv")

# Getting lower and upper cred intervals ----
for (i in 1:nrow(data)){
  data$Lower.CI[i]=binom.test(data$Bd.[i],data$Samples..n.[i])$conf.int[1]
  data$Upper.CI[i]=binom.test(data$Bd.[i],data$Samples..n.[i])$conf.int[2]
}

# calculate probability of getting no positives ----
# Using the endemism rate of 11% from Talley et al. (2011)
data$Pr..no.Bd. = dbinom(0,data$Samples..n.,.11)
write.csv(data,'PowerAnalTable.csv')
