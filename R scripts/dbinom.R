library(reshape2)
mu = read.csv("/Users/hasansulaeman/Dropbox/Costa Rica Bd Project/Data/S.table.1.csv")
mu$Negatives = mu$Sample.Size - mu$Positives
mu$lower=NA
mu$upper=NA

for (i in 1:nrow(mu)){
  mu$lower[i]=binom.test(mu$Positives[i],mu$Sample.Size[i])$conf.int[1]
  mu$upper[i]=binom.test(mu$Positives[i],mu$Sample.Size[i])$conf.int[2]
}

# calculate probability of getting no positives, given an expected detection rate of 11%
mu.dec2$prob =  dbinom(0,mu.dec2$tot,.11)
write.csv(mu,'s1Table_HS.csv')