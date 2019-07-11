#---

#New England lpj-guess test run
#Charlotte Uden
#6/25/2019

#---


#The first test run used global PFTs and ran using New England climate data from 1901-2014. IT produced six files: anpp.out (annual net primary production), cmass.out (carbon mass), cpool.out (carbol pool -pgC, petagrams of carbon?), dens.out, firert.out, height.out. This script will includes a few funcitons for cleaning up and visualizing the data. 

#Global PFT's definitions (taken from LPJ-GUESS homepage):


#https://github.com/joergsteinkamp/RLPJGUESS -this might be useful (someone else's code for dealing with lpj-guess output)

#-----------------------------------------------------------------------------
##-----------anpp: Annual Net Pimary Production-------------------------------

library(dplyr)
library(ggplot2)
library(reshape2) #use melt function
library(scatterpie) #make map of pie charts



anpp <- read.table("/Users/charlotteuden/Desktop/trees/LPJ-GUESS/NewEngland_lpj-guessTestRun/anpp.out", stringsAsFactors=FALSE)

head(anpp) #column names are weird -first row should be col names. all data are factors
str(anpp)

####################
# FUNCTION: outputCleanUp
# INPUTS: output from LPJ-GUESS (ie. anpp.out). this is specific to global PFT's.
# OUTPUTS: correctly named columns with numeric data
#------------------------------
outputCleanUp <- function(table=matrix(data=rep(0, 64), ncol=16)){
  table <- data.frame(table)
  names(table) <- c("Lon","Lat","year","BNE","BINE","BNS","TeNE","TeBS","IBS","TeBE","TrBE","TrIBE","TrBR","C3G","C4G","Total")#rename columns
  table <- table[-c(1),] #remove first row
  table[, 1:16] <- lapply(table[, 1:16], as.numeric) #change data from factors to numberic
  return(table)
}
#------------------------------
anpp <- outputCleanUp(table=anpp)
head(anpp)


#BNE for 1901 (boreal needleleaf evergreen shade tolerant)
BNE1901anpp <- ggplot(filter(anpp, year == 1901) , mapping=aes(x=Lon, y=Lat, color=BNE)) + 
  geom_point(size=10, alpha=0.9) + 
  ggtitle("BNE net primary production in 1901") +
  scale_color_gradient(low="yellow", high="blue")
#BNE for 2014
BNE2014anpp <- ggplot(filter(anpp, year == 2014) , mapping=aes(x=Lon, y=Lat, color=BNE)) + 
  geom_point(size=10, alpha=0.9) + 
  ggtitle("BNE net primary production in 2014") +
  scale_color_gradient(low="yellow", high="blue")


#In order to show change in species composition over time, we need to 'melt' our anpp table into a three column table: year, PFT, sum of annual net primary produciton of all coordinate points in that year. 


####################
# FUNCTION: PFTmelt
# INPUTS: 16 column table of plant funcitonal types for each coordinate point, for each year
# OUTPUTS: 3 column table: year, PFT, sum of annual net primary produciton of all coordinate points in that year. 
#------------------------------
PFTmelt <- function(table=x){
  table <- melt(data = table, id.vars = "year", measure.vars = c('BNE',  'BINE', 'BNS', 'TeNE', 'TeBS', 'IBS', 'TeBE', 'TrBE', 'TrIBE', 'TrBR',   'C3G', 'C4G'))
  table <- with(table, aggregate(value, by = list(year, variable), 
                                 FUN = "sum")) #find sum of anpp for that PFT across all coordinate points
  names(table) <- c("year", "PFT", "sum.value") #rename columns
  return(table)
}
#------------------------------

anppMelt <- PFTmelt(table=anpp)

chiz <- c("#ef8834","#efb734", "#2366d1", "#05723e", "#069b7d","#cc2d0a", "#4f6496", "#99501d", "#bbeaed", "#f7c640", "#8cdcea", "#8c991d") #color palett for my plot

anppTime <- ggplot(anppMelt, aes(x=year, y=sum.value, fill=PFT)) +
  geom_area(alpha=1) +
  scale_fill_manual(values=chiz) +
  ggtitle("anual net primary production")


#Spatial representation of species composition for 1901 and 2014

map1901anpp <- ggplot() + 
  geom_scatterpie(aes(x=Lon, y=Lat), data=filter(anpp, year == 1901), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) +
  coord_fixed() + 
  #scale_fill_manual(values=chiz) + 
  ggtitle("1901: anual net primary production")

map2014anpp <- ggplot() + 
  geom_scatterpie(aes(x=Lon, y=Lat), data=filter(anpp, year == 2014), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) +
  coord_fixed() + 
  #scale_fill_manual(values=chiz) + 
  ggtitle("2014: anual net primary production")

BNE1901anpp
BNE2014anpp
map1901anpp
map2014anpp
anppTime

#-----------------------------------------------------------------------------
##------------------cmass: Carbon Mass------------------------


#read in data:
cmass <- read.table("NewEngland_lpj-guessTestRun/cmass.out", stringsAsFactors=FALSE)

#head(cmass) 
#str(cmass) #looks like the same problem: non numerica data, columns incorrectly labeled

#clean up data
cmass <- outputCleanUp(table=cmass)
cmassMelt <- PFTmelt(table=cmass) #data arrangement appropriate for temporal visualization 

#temporal representation of PFT composition
cmassTime <- ggplot(cmassMelt, aes(x=year, y=sum.value, fill=PFT)) +
  geom_area(alpha=1) +
  scale_fill_manual(values=chiz) +
  ggtitle("carbon mass")


#Spatial representation of PFT composition
map1901cmass <- ggplot() + 
  geom_scatterpie(aes(x=Lon, y=Lat), data=filter(cmass, year == 1901), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) +
  coord_fixed() + 
  #scale_fill_manual(values=chiz) + 
  ggtitle("1901: carbon mass")

map2014cmass <- ggplot() + 
  geom_scatterpie(aes(x=Lon, y=Lat), data=filter(cmass, year == 2014), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) +
  coord_fixed() + 
  #scale_fill_manual(values=chiz) + 
  ggtitle("2014: carbon mass")

map1901cmass
map2014cmass
cmassTime

#-----------------------------------------------------------------------------
##--------------------cpool: Carbon Pool--------------------

#Seven columns: Lon, Lat, Year, VegC (carbon in vegetation), LitterC, SoilC and Total. I'm not 100% sure, but I think the unit is petagrams of carbon (pgC).



#read in data:
cpool <- read.table("NewEngland_lpj-guessTestRun/cpool.out", stringsAsFactors=FALSE)

#same problem (column names are wrong and non numerica data) but need to use different names. 

####################
# FUNCTION: cpoolCleanUp
# INPUTS: cpool.out data from LPJ-GUESS. 
# OUTPUTS: correctly named columns with numeric data
#------------------------------
cpoolCleanUp <- function(table=x){
table <- data.frame(table)
names(table) <- c("Lon","Lat","year","VegC","LitterC","SoilC","Total")#rename columns
table <- table[-c(1),] #remove first row
table[, 1:7] <- lapply(table[, 1:7], as.numeric) #change data from factor to numeric
return(table)
}
#------------------------------

####################
# FUNCTION: cpoolmelt
# INPUTS: 7 column table of carbon pool data for each coordinate point, for each year
# OUTPUTS: 3 column table: year, carbon pool, sum of carbon for that pool type (veg, soil, litter) for all coordinate points in that year. 
#------------------------------
cpoolmelt <- function(table=x){
table <- melt(data = table, id.vars = "year", measure.vars = c("VegC","LitterC","SoilC"))
table <- with(table, aggregate(value, by = list(year, variable), 
FUN = "sum")) #find sum of anpp for that PFT across all coordinate points
names(table) <- c("year", "type", "sum.value") #rename columns
return(table)
}
#------------------------------
#clean up data
cpool <- cpoolCleanUp(table=cpool)
cpoolMelt <- cpoolmelt(table=cpool) #data arrangement appropriate for temporal visualization 

#temporal representation of carbon pools 
cpoolTime <- ggplot(cpoolMelt, aes(x=year, y=sum.value, fill=type)) +
geom_area(alpha=1) +
scale_fill_manual(values=chiz) +
ggtitle("Carbon Pool")#not that interesting


#Spatial representation of PFT composition
map1901cpool <- ggplot() + 
geom_scatterpie(aes(x=Lon, y=Lat), data=filter(cpool, year == 1901), cols=c("VegC","LitterC","SoilC")) +
coord_fixed() + 
#scale_fill_manual(values=chiz) + 
ggtitle("1901: carbon pools")

map2014cpool <- ggplot() + 
geom_scatterpie(aes(x=Lon, y=Lat), data=filter(cpool, year == 2014), cols=c("VegC","LitterC","SoilC")) +
coord_fixed() + 
#scale_fill_manual(values=chiz) + 
ggtitle("2014: carbon pools")

map1901cpool
map2014cpool
cpoolTime


#-----------------------------------------------------------------------------
##--------------------dens: tree density --------------------


#read in data:
dens <- read.table("NewEngland_lpj-guessTestRun/dens.out", stringsAsFactors=FALSE)
#head(dens)

#same problem (column names are wrong and non numerica data) but need to use different names. 

dens <- outputCleanUp(table=dens)
densMelt <- PFTmelt(table=dens) #data arrangement appropriate for temporal visualization 

#temporal representation of PFT composition
densTime <- ggplot(densMelt, aes(x=year, y=sum.value, fill=PFT)) +
geom_area(alpha=1) +
scale_fill_manual(values=chiz) +
ggtitle("dens.out")


#Spatial representation of PFT composition
map1901dens <- ggplot() + 
geom_scatterpie(aes(x=Lon, y=Lat), data=filter(dens, year == 1901), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) +
coord_fixed() + 
#scale_fill_manual(values=chiz) + 
ggtitle("1901: dens.out")

map2014dens <- ggplot() + 
geom_scatterpie(aes(x=Lon, y=Lat), data=filter(dens, year == 2014), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) +
coord_fixed() + 
#scale_fill_manual(values=chiz) + 
ggtitle("2014: dens.out")

map1901dens
map2014dens
densTime


#-----------------------------------------------------------------------------
##--------------------firert: fire frequency--------------------

#Four columns: Lon, Lat, Year, FireRT. I am not sure what FireRT is. 



#read in data:
firert <- read.table("NewEngland_lpj-guessTestRun/firert.out", stringsAsFactors=FALSE)

####################
# FUNCTION: firertCleanUp
# INPUTS: firert.out data from LPJ-GUESS. 
# OUTPUTS: correctly named columns with numeric data
#------------------------------
firertCleanUp <- function(table=x){
table <- data.frame(table)
names(table) <- c("Lon","Lat","year","FireRT")#rename columns
table <- table[-c(1),] #remove first row
table[, 1:4] <- lapply(table[, 1:4], as.numeric) #change data from factor to numeric
return(table)
}
#------------------------------

####################
# FUNCTION: firertmelt
# INPUTS: 4 column table of firert data for each coordinate point, for each year
# OUTPUTS: 2 column table: year, mean value for FireRT in that year
#------------------------------
firertmelt <- function(table=x){
table <- melt(data = table, id.vars = "year", measure.vars = c("FireRT"))
table <- with(table, aggregate(value, by = list(year, variable), 
FUN = "mean")) #find sum of anpp for that PFT across all coordinate points
names(table) <- c("year", "NA", "mean.firert") #rename columns
return(table)
}
#------------------------------

#clean up data
firert <- firertCleanUp(table=firert)
firertMelt <- firertmelt(table=firert)

#temporal representation of PFT composition
firertTime <- ggplot(firertMelt, aes(x=year, y=mean.firert)) +
geom_line() +
ggtitle("Mean FireRT")


#Spatial representation of PFT composition
firert1901 <- ggplot(filter(firert, year == 1901) , mapping=aes(x=Lon, y=Lat, color=FireRT)) + 
geom_point(size=10, alpha=0.9) + 
ggtitle("FireRT for 1901") +
scale_color_gradient(low="yellow", high="blue")

firert2014 <- ggplot(filter(firert, year == 2014) , mapping=aes(x=Lon, y=Lat, color=FireRT)) + 
geom_point(size=10, alpha=0.9) + 
ggtitle("FireRT for 2014") +
scale_color_gradient(low="yellow", high="blue")


firert1901
firert2014
firertTime

#-----------------------------------------------------------------------------
##--------------------height: tree height--------------------


#read in data:
height <- read.table("NewEngland_lpj-guessTestRun/height.out", stringsAsFactors=FALSE)

####################
# FUNCTION: heightCleanUp
# INPUTS: height output from LPJ-GUESS. this is specific to global PFT's.
# OUTPUTS: correctly named columns with numeric data
#------------------------------
heightCleanUp <- function(table=x){
  table <- data.frame(table)
  names(table) <- c("Lon","Lat","year","BNE","BINE","BNS","TeNE","TeBS","IBS","TeBE","TrBE","TrIBE","TrBR","C3G","C4G")#rename columns
  table <- table[-c(1),] #remove first row
  table[, 1:15] <- lapply(table[, 1:15], as.numeric) #change data from factors to numberic
  return(table)
}
#------------------------------

####################
# FUNCTION: heightmelt
# INPUTS: 15 column table of heights for each PFT for each coordinate point, for each year
# OUTPUTS: 3 column table: year, PFT, mean of heights at all coordinate points in that year. 
#------------------------------
heightmelt <- function(table=x){
  table <- melt(data = table, id.vars = "year", measure.vars = c('BNE',  'BINE', 'BNS', 'TeNE', 'TeBS', 'IBS', 'TeBE', 'TrBE', 'TrIBE', 'TrBR',   'C3G', 'C4G'))
  table <- with(table, aggregate(value, by = list(year, variable), 
                                 FUN = "mean")) #find sum of anpp for that PFT across all coordinate points
  names(table) <- c("year", "PFT", "mean.value") #rename columns
  return(table)
}
#------------------------------


#clean up data
height <- heightCleanUp(table=height)
heightMelt <- heightmelt(table=height) 

#temporal representation of PFT composition
heightTime <- ggplot(heightMelt, aes(x=year, y=mean.value, fill=PFT)) +
  geom_area(alpha=1) +
  scale_fill_manual(values=chiz) +
  ggtitle("height")


#Spatial representation of PFT composition
map1901height <- ggplot() + 
  geom_scatterpie(aes(x=Lon, y=Lat), data=filter(height, year == 1901), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) +
  coord_fixed() + 
  #scale_fill_manual(values=chiz) + 
  ggtitle("1901: height")

map2014height <- ggplot() + 
  geom_scatterpie(aes(x=Lon, y=Lat), data=filter(height, year == 2014), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) +
  coord_fixed() + 
  #scale_fill_manual(values=chiz) + 
  ggtitle("2014: height")

map1901height
map2014height
heightTime

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#----------------------------adding information to the maps-----------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

library(tmap)
library(mapdata)
library(maps)
library(ggplot2)
library(dplyr)

#----------------------State Boundaries------------------------------------------------------

states <- map_data("state")#turn state line map into data frame
northeast <- subset(states, region %in% c("vermont", "new hampshire", "connecticut", "maine", "rhode island", "massachusetts", "new york"))#subest new england states

#map with states and lpj-guess output: anpp
map <- ggplot() + 
  geom_polygon(data = northeast, aes(x=long, y = lat, group = group), fill='gray70', color='white') +
  coord_fixed(1.3) +
  geom_scatterpie(aes(x=Lon, y=Lat), data=filter(anpp, year == 1901), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G'), color='white') +
  coord_fixed() + 
  scale_fill_manual(values=chiz) + 
  ggtitle("1901: anual net primary production")

map

#--------------converting data frames to a structural feature (sf) object---------------------------

#I need to convert my data frame into an sf (simple featrues) object. sf objects can be treated as a regular data frame, but have a geometry column -coordinates. Like a 'spatial data frame' the package tmap uses sf objects, not regular old data frames. 

library(sf)
library(raster)
library(dplyr)
library(stringr) # for working with strings (pattern matching)

str(anpp) #data frame

####################
# FUNCTION: crsConvert
# INPUTS: output from LPJ-GUESS 
# OUTPUTS: data frame converted to structural feature object with lat and long converted to WGS 84 coordinate system
#------------------------------
sfConvert <- function(table=x){
  #use st_as_sf() to convert data frame to sf object: 
  table <- st_as_sf(table, coords = c("Lon", "Lat"), 
                      crs = 26919, agr = "constant") 
  #crs 26919 refers to UTM NAD83 zone 19N, but you can set the coordiate reference system to whatever. 
  return(table)
} 

#------------------------------

anpp_sf <- sfConvert(table=anpp)

st_crs(anpp_sf) #tells you what coordinate reference system the sf is in. 
#the cool thing about potting sf objects is that by default, all attributes will individually be plotted:
plot(anpp_sf)

#convert polygons for new england to sf objects: 
northeast_sf <- st_as_sf(new_england, coords = c("long", "lat"), 
                  crs = 26919, agr = "constant")

#using package tmap:
state_outline <- tm_shape(northeast_sf) + tm_dots(col="grey", border.col="white")# this is not a polygon shapefile...
state_outline + tm_shape(anpp_sf) + tm_dots(col="BINE", size=1, style="fisher")

#using just the plot fuction
plot(northeast_sf)
plot(anpp_sf)

#using ggplot:
ggplot(data=northeast_sf) + geom_sf() + geom_sf(data=anpp_sf)

#can i mix sf objects and reguar data frams in one plot?
ggplot() + geom_polygon(data = northeast, aes(x=long, y = lat, group = group), fill='gray70', color='white') + geom_sf(data=anpp_sf) #yes!

#I'm starting to think that ggplot is better than tmap... at least for what I'm doing. 

library("ggspatial") #for north arrow and scale bar
#change crs of world dataset: 
world_sf <- st_as_sf(world, coords = c("long", "lat"), 
                         crs = 26919, agr = "constant")


data <- ggplot() + geom_scatterpie(aes(x=Lon, y=Lat), data=filter(anpp, year == 1901), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G'), color='white') +
  coord_fixed() + 
  scale_fill_manual(values=chiz) + 
  ggtitle("1901: anual net primary production")

data + geom_sf(data=world_sf, color = "white", fill = "gray70", alpha=0.2) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-80,-66), ylim = c(40,48), expand = FALSE)





#--------------ELEVATION-------------------

#Uhttps://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1-arc?qt-science_center_objects=0#qt-science_center_objects

require(rgdal)
require(maptools)
require(raster)

myproj  <- "+proj=utm +zone=12 +north +ellps=WGS84 +units=m"
elev <- st_read("/Users/charlotteuden/Desktop/trees/LPJ-GUESS/srtm.shp")
st_geometry_type(elev)
plot(elev)
ras = raster(elev)


demo <- st_read("/Users/charlotteuden/Desktop/trees/LPJ-GUESS/srtm.shp") # Creates a SpatialPolygonsDataFrame class (sp)
r <- raster(ncol=2, nrow=1)
extent(r) <- extent(demo)

