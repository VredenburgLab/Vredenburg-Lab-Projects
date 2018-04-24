# Analysis of Marina's museum data

# read data

mu.all = read.table('museumdata.txt',header=T,sep='\t',na.strings=c('N/D')) 

# get just RACA data

mu = subset(mu.all, species == 'Rana   cascade')

# remove samples that are probably not RACA, or have no date information
  # one from right outside of Chico
  # 4 near Lewiston on Trinity River

mu = mu[-which(mu$sampleName%in% c(1020,238915:238918) | is.na(mu$year)),]

# create decade column

mu$decade = ifelse(mu$year<1910,'00s',ifelse(mu$year>1909&mu$year<1920,'10s',ifelse(mu$year>1919&mu$year<1930,'20s',ifelse(mu$year>1929&mu$year<1940,'30s',ifelse(mu$year>1939&mu$year<1950,'40s',ifelse(mu$year>1949&mu$year<1960,'50s',ifelse(mu$year>1959&mu$year<1970,'60s',ifelse(mu$year>1969&mu$year<1980,'70s',ifelse(mu$year>1989&mu$year<2000,'90s',ifelse(mu$year>1999&mu$year<2010,'2000s',NA))))))))))

# create decade table

library(reshape2)

mu.dec = dcast(data=mu,decade~Bdpres,value.var='year',fun.aggregate=length)
names(mu.dec)[c(2,3)] = c('neg','pos')

mu.dec$tot = mu.dec$neg+mu.dec$pos

mu.dec$lower = NA
mu.dec$upper= NA

for (i in 1:nrow(mu.dec)){
  mu.dec$lower[i]=binom.test(mu.dec$pos[i],mu.dec$tot[i])$conf.int[1]
  mu.dec$upper[i]=binom.test(mu.dec$pos[i],mu.dec$tot[i])$conf.int[2]
}

# calculate probability of getting no positives, given an expected detection rate of 11%

mu.dec$prob =  dbinom(0,mu.dec$tot,.11)


# write table

write.csv(mu.dec,'DecadeTable.csv')

# calculate CI for total

  binom.test(13,368)

# create county table

mu.co = dcast(data=mu,county~Bdpres,value.var='year',fun.aggregate=length)
names(mu.co)[c(2,3)] = c('neg','pos')

mu.co$tot = mu.co$neg+mu.co$pos

mu.co$lower = NA
mu.co$upper= NA

for (i in 1:nrow(mu.co)){
  mu.co$lower[i]=binom.test(mu.co$pos[i],mu.co$tot[i])$conf.int[1]
  mu.co$upper[i]=binom.test(mu.co$pos[i],mu.co$tot[i])$conf.int[2]
}

write.csv(mu.co,'CountyTable.csv')

# create year table

mu.yr = dcast(data=mu,year~Bdpres,value.var='decade',fun.aggregate=length)
names(mu.yr)[c(2,3)] = c('neg','pos')

mu.yr$tot = mu.yr$neg+mu.yr$pos

mu.yr$yrc = mu.yr$year-1955

write.csv(mu.yr,'YearTable.csv')

# Estimate time of arrival
  # use jags and rjags

library(rjags)

# establish data

inf = mu.yr$pos
n = mu.yr$tot
time = mu.yr$yrc
nrows = nrow(mu.yr)

# create model

model_string = 'model{
  for(ii in 1:nrows){
    inf[ii]~dbin(p[ii],n[ii])
    p[ii]<-ppres[ii]*mu
    ppres[ii]<-step(tst[ii])
    tst[ii]<-time[ii]-arr.time
  }
  arr.time~dunif(-48,48)
  mu~dbeta(1,1)
}'

arr.time <- jags.model(textConnection(model_string), data = list('inf' = inf,'n' = n, 'time' = time,  'nrows'=nrows), n.chains = 3, n.adapt = 10000)


update(arr.time,10000)

arr.time_samples = coda.samples(arr.time,c('arr.time','mu'),n.iter=20000)

plot(arr.time_samples)

summary(arr.time_samples)



########################################