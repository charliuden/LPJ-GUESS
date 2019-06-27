#New England lpj-guess test run


#-----------------------------------
  #------anpp-----------annual net primary production
#-----------------------------------


anpp <- read.table("/Users/charlotteuden/Desktop/trees/data/New England lpj-guess test run/anpp.out", stringsAsFactors=FALSE)

head(anpp) #column names are weird -first row should be col names. all data are factors
str(anpp)
library(dplyr)
anpp2 <- rename(anpp, Lon=V1, Lat=V2, year=V3, BNE=V4, BINE=V5, BNS=V6, TeNE=V7, TeBS=V8, IBS=V9, TeBE=V10, TrBE=V11, TrIBE=V12, TrBR=V13, C3G=V14, C4G=V15, Total=V16)#rename columns
anpp2 <- anpp2[-c(1),] #remove first row
anpp2[, 1:16] <- lapply(anpp2[, 1:16], as.numeric) #change data from factors to numberic
head(anpp2)
str(anpp2)

#BNE for 1901 (boreal needleleaf evergreen shade tolerant)
library(ggplot2)
BNE1901 <- ggplot(filter(anpp2, year == 1901) , mapping=aes(x=Lon, y=Lat, color=BNE)) + 
  geom_point(size=10, alpha=0.9) + 
  ggtitle("BNE 1901") +
  scale_color_gradient(low="yellow", high="blue")
#BNE for 2014
BNE2014 <- ggplot(filter(anpp2, year == 2014) , mapping=aes(x=Lon, y=Lat, color=BNE)) + 
  geom_point(size=10, alpha=0.9) + 
  ggtitle("BNE 2014") +
  scale_color_gradient(low="yellow", high="blue")
BNE1901
BNE2014

library(reshape2)
anpp3 <- melt(data = anpp2, id.vars = "year", measure.vars = c('BNE',  'BINE', 'BNS', 'TeNE', 'TeBS',   'IBS', 'TeBE', 'TrBE', 'TrIBE', 'TrBR',   'C3G', 'C4G'))
head(anpp3)
anpp4 <- with(anpp3, aggregate(value, by = list(year, variable), 
                                    FUN = "mean"))
names(anpp4) <- c("year", "PFT", "mean.prop")
head(anpp4)#data frame of meam proportions of each PFT for each year

chiz <- c("#ef8834","#efb734", "#2366d1", "#05723e", "#069b7d","#cc2d0a", "#4f6496", "#99501d", "#bbeaed", "#f7c640", "#8cdcea", "#8c991d") 

timePFT <- ggplot(anpp4, aes(x=year, y=mean.prop, fill=PFT)) +
  geom_area(alpha=1) +
  scale_fill_manual(values=chiz)
  

library(scatterpie)

map1901 <- ggplot() + 
  geom_scatterpie(aes(x=Lon, y=Lat), data=filter(anpp2, year == 1901), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) + 
  coord_fixed() + 
  #scale_fill_manual(values=chiz) + 
  ggtitle("1901")

map2014 <- ggplot() + 
  geom_scatterpie(aes(x=Lon, y=Lat), data=filter(anpp2, year == 2014), cols=c('BNE','BINE','BNS','TeNE','TeBS','IBS','TeBE','TrBE','TrIBE','TrBR','C3G','C4G')) + 
  coord_fixed() + 
  #scale_fill_manual(values=chiz) + 
  ggtitle("2014")

BNE1901
BNE2014
map1901
map2014
timePFT




anpp1901melt <- melt(data = anpp1901, id.vars = c("year","Lat", "Lon"), measure.vars = c('BNE',  'BINE', 'BNS', 'TeNE', 'TeBS',   'IBS', 'TeBE', 'TrBE', 'TrIBE', 'TrBR',   'C3G', 'C4G'))
names(anpp1901melt) <- c("year", "Lat", "Lon", "PFT", "value")
head(anpp1901melt)


#-----------------------------------
#------cmass-----------
#-----------------------------------


cmass <- read.table("/Users/charlotteuden/Desktop/trees/data/New England lpj-guess test run/cmass.out", stringsAsFactors=FALSE)

head(cmass)


#add topography to spatial one