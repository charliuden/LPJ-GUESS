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



anpp <- read.table("New England lpj-guess test run/anpp.out", stringsAsFactors=FALSE)

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
cmass <- read.table("New England lpj-guess test run/cmass.out", stringsAsFactors=FALSE)

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
cpool <- read.table("New England lpj-guess test run/cpool.out", stringsAsFactors=FALSE)

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
dens <- read.table("New England lpj-guess test run/dens.out", stringsAsFactors=FALSE)
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
firert <- read.table("New England lpj-guess test run/firert.out", stringsAsFactors=FALSE)

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
height <- read.table("New England lpj-guess test run/height.out", stringsAsFactors=FALSE)

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
