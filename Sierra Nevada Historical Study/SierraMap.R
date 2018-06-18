
# ++++++++++++++++++++++++++++++++++++++++++++++
# Sierra Nevada Picking Points within a Polygon
# ++++++++++++++++++++++++++++++++++++++++++++++

# Library and data setup
library(rgdal)
library(sp)
library(raster) 
library(spatialEco)
SR = readOGR(dsn = "./Shapefile", "a_SierraNevada_extract")
SR = spTransform(SR, CRS="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# Specify the folder name, don't take shortcuts
# Don't specify any individual shapefile name, just write down the name of the files
data = read.csv("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data/rawdata.csv")

# Visualize the points ----
for (i in 1:nrow(data)) {
  data[i,]$longitude = as.numeric(format(data[i,]$longitude, digits = 5))
  data[i,]$latitude = as.numeric(format(data[i,]$latitude, digits = 5))
}
coordinates(data) = ~ Longitude + Latitude
proj4string(data) = proj4string(SR)

plot(SR)
plot(data, add=T)

inside.park = !is.na(over(data, as(SR, "SpatialPolygons")))
table(inside.park)
