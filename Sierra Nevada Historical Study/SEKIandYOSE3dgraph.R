
# MAKING A 3D GRAPH FOR LATITUDE, ZSCORE, TIME, AND PREVALENCE

# Setting up the library and data ----
library(plot3D)
library(plot3Drgl)
library(magick)
library(gridExtra)
data = read.csv("/Users/hasansulaeman/Documents/GitHub/Publications/
                Sierra Nevada Historical Study/3dgraphyoseseki.csv")

# Set up the variables ----
x = data$longitude
y = data$latitude
z = data$year
a = log10(data$Zscore+1)

# Plotting the positives with points in the background ----
scatter3D(x, y, z, pch = 20, col.var = as.factor(data$group), 
          col = c("red", "green"), ticktype = "detailed", 
          theta = -60, phi=20, d = 2, bty = "u", cex = 2, 
          col.panel ="steelblue", expand =0.5, col.grid = "darkblue", 
          xlab="Longitude", ylab="Latitude", 
          zlab="Time (Year)", xlim=c(-121,-117))
# To look at it interactively, run the code below after plotting
plotrgl()
movie3d(spin3d(), duration = 10, fps = 100)

neg = subset(data, data$group=="negative")
pos = subset(data, data$group=="positive")
a = log10(pos$Zscore+1)

par(c(2,4,4,2))
x = neg$longitude
y = neg$latitude
z = neg$year
scatter3D(x, y, z, pch = 20, ticktype = "detailed", col = "azure3",
          theta = -60, phi=20, d = 2, bty = "u", cex = 2, 
          col.panel ="steelblue", expand =0.5, col.grid = "darkblue", 
          xlab="Longitude", ylab="Latitude", 
          zlab="Time (Year)", xlim=c(-121,-117))
x = pos$longitude
y = pos$latitude
z = pos$year
scatter3D(x, y, z, pch = 20, ticktype = "detailed", theta = -60, colvar = a,
          phi=20, d = 2, bty = "u", cex = 2, col.panel ="steelblue", expand =0.5,
          col.grid = "darkblue", xlab="Longitude", ylab="Latitude", 
          col=ramp.col(c("yellow","orange","red")),zlab="Time (Year)", 
          xlim=c(-121,-117), add=TRUE, colkey = list(side = 4, length = 0.5, 
          dist=0.01), clab="Zscore")

data$zscore = log10(data$zscore+1)
q=ggplot(data, aes(x=latitude, y=year, color= zscore>0, shape=group)) +
  geom_point() + 
  theme(legend.position = "none")
q
q + stat_ellipse(type="norm")

# Plotting marginal X-density
xdensity <- ggplot(data, aes(latitude, fill=group)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
xdensity

# Plotting marginal Y-density
ydensity = ggplot(seki, aes(year)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values ='#999999') + 
  theme(legend.position = "none")
ydensity

# Putting the Plots together. First create a blank plot
blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

# Grid it up
grid.arrange(xdensity, blankPlot, q, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

# Trying out some prevalence graphs
seki = subset(data, group=="seki")
yose = subset(data, group=="yose")
sum.seki = dcast(data=seki,year~status,value.var='status',fun.aggregate=length)
sum.seki$prevalence = 0
for (i in 1:nrow(sum.seki)) {
  sum.seki[i,]$prevalence = sum.seki[i,3]/(sum.seki[i,2]+sum.seki[i,3])  
}
sum.yose = dcast(data=yose,year~status,value.var='status',fun.aggregate=length)
sum.yose$prevalence = 0
for (i in 1:nrow(sum.yose)) {
  sum.yose[i,]$prevalence = sum.yose[i,3]/(sum.yose[i,2]+sum.yose[i,3])  
}

sum.seki$group = "seki"
sum.yose$group = "yose"
sum = rbind(sum.yose, sum.seki)

sum = read.csv(file.choose())
ggplot(sum, aes(x=year, y=y.prev)) + 
  geom_line(alpha=.5, color = "red") +
  geom_line(aes(y=s.prev), alpha=.5, color="blue")
prevline


write.csv(sum.seki, "sekisummary.csv")
