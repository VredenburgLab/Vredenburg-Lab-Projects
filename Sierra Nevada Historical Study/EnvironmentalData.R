
# RETRIEVING ENVIRONMENTAL DATA
# Hasan Sulaeman
# 5/9/2018

# Library and Working Directory Setup ----
data=read.csv("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data/sekidata.csv", header=TRUE)
library(FedData)         # Package to get data from USGS national hydrography dataset
library(raster)          # Package to get data from worldclim
library(rgeos)           # Package to work with geospatial data
library(measurements)    # Package to convert measurements

# (IF NEEDED) Convert from decimal minutes to decimal degrees ----
data$Latitude = measurements::conv_unit(data$Latitude, from = 'deg_dec_min', to = 'dec_deg')
data$Longitude = measurements::conv_unit(data$Longitude, from = 'deg_dec_min', to = 'dec_deg')

# Converting the coordinates into a spatial class ----
x = data.frame(x=data$longitude,y=data$latitude)   
sp = SpatialPointsDataFrame(coords = x, data = x,
     proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# NOTE: US Federal Agencies use ESGS 4269
# +init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0 

# Retrieving WorldClim data ----
bio = getData("worldclim", var="bio", res=10)           # We used the function getData to fetch "bio" variables from worldclim                   
bio = bio[[c(1:19)]]                                    # We're fetching 19 bio variables for temperature and precipitation
colnames(x)=c("longitude", "latitude")
values = (extract(bio,x))                               # We're saving the values
data=cbind.data.frame(data, values)
write.csv(data, "rawdata.csv")

# Venter et al. data on anthropogenic effects
# Note that I excluded lights cause I don't think it's ever important for what we do
HFP=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/HFP2009.tif")
crop=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Croplands2005.tif")
built=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Built2009.tif")
navwat=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Navwater2009.tif")
popden=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Popdensity2010.tif")
road=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Roads.tif")
rail=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Railways.tif")
pasture=raster("/Users/hasansulaeman/Google Drive/HumFootprint/Maps/Pasture2009.tif")
anthro=stack(HFP, crop, built, navwat, popden, road, rail, pasture)
ay=extract(anthro, sp)
data=cbind.data.frame(data, ay) # Adding the retrieved data into the data frame

# NHD data. Still working on it.. ----
nhd=get_nhd(template=sp, label="distance to waterbody", extraction.dir="/Users/hasansulaeman/Documents/GitHub/Publications/Sierra Nevada Historical Study", 
            raw.dir="/Users/hasansulaeman/Documents/GitHub/Publications/Sierra Nevada Historical Study")
crs(nhd$`_Waterbody`)=CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
crs(nhd$`_Flowline`)=CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
crs(sp)=CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")

# Fetch the data sets for water bodies and flowlines
waterbody=nhd$`_Waterbody`
flowline=nhd$`_Flowline`

# Break down each type of water body depending on their FCode
playa=subset(waterbody, waterbody$FCode==36100)
icemass=subset(waterbody, waterbody$FCode==37800)
lakepond=subset(waterbody, waterbody$FCode %in% c(39000:39012))
reservoir=subset(waterbody, waterbody$FCode %in% c(43600:43626))
swamp=subset(waterbody, waterbody$FCode %in% c(46600:46601))
estuary=subset(waterbody, waterbody$FCode==49300)

# Break down each type of flowline depending on their FCode
canalditch=subset(flowline, flowline$FCode %in% c(33600:3363))
intermittentstream=subset(flowline, flowline$FCode==46003)
perennialstream=subset(flowline, flowline$FCode==46006)
ephemeralstream=subset(flowline, flowline$FCode==46007)

# Now we can go and get distances
# Distance to Playa
distplaya=gDistance(spgeom1=sp, spgeom2=playa, byid=T)
distplaya=apply(distplaya,2,min)
distplaya=distplaya*1000
data$distplaya=cbind(data, distplaya)

# Distance to Ice Mass
disticemass=gDistance(spgeom1=sp, spgeom2=icemass, byid=T)
disticemass=apply(disticemass,2,min)
disticemass=disticemass*1000
data=cbind(data, disticemass)

# Distance to Lake/Pond
distlakepond=gDistance(spgeom1=sp, spgeom2=lakepond, byid=T)
distlakepond=apply(distlakepond,2,min)
data=cbind(data, distlakepond)

# Distance to Reservoir
distreservoir=gDistance(spgeom1=sp, spgeom2=reservoir, byid=T)
distreservoir=apply(distreservoir,2,min)
data=cbind(data, distreservoir)
plot(data$bio1, data$bio2)
# Distance to Swamp
distswamp=gDistance(spgeom1=sp, spgeom2=swamp, byid=T)
distswamp=apply(distswamp,2,min)
data=cbind(data, distswamp)

# Distance to Estuary
distestuary=gDistance(spgeom1=sp, spgeom2=estuary, byid=T)
distestuary=apply(distestuary,2,min)
data=cbind(data, distestuary)

# Distance to Canal/Ditch
distcanalditch=gDistance(spgeom1=sp, spgeom2=canalditch, byid=T)
distcanalditch=apply(distcanalditch,2,min)
data=cbind(data, distcanalditch)

# Distance to Intermittent Streams
distintermittentstream=gDistance(spgeom1=sp, spgeom2=intermittentstream, byid=T)
distintermittentstream=apply(distintermittentstream,2,min)
data=cbind(data, distintermittentstream)

# Distance to Perennial Streams
distperennialstream=gDistance(spgeom1=sp, spgeom2=perennialstream, byid=T)
distperennialstream=apply(distperennialstream,2,min)
data=cbind(data, distperennialstream)

# Distance to Ephemeral Streams
distephemeralstream=gDistance(spgeom1=sp, spgeom2=ephemeralstream, byid=T)
distephemeralstream=apply(distephemeralstream,2,min)
data=cbind(data, distephemeralstream)

res(nhd)
