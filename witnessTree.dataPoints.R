

#---
#Forest Composition in the Northeastern United States at the time of European Settlement
#Charlotte Uden
#5/10/2019
#---

library(mapdata)
library(maps)
library(ggplot2)
library(dplyr)
  
NE <- read.csv("witness_tree/583NewEngland%.csv")
NY <- read.csv("witness_tree/NY%327presett.csv")

#change 'X' colname to 'unit' in NY dataset
NY <- rename(NY, unit = X)

#remove 'other' column for NY dataset
NY <- select(NY, Ver..2010.09:Magnolia)

#remove % character from values in NY dataset
cleanUp <- function(data=x){
  data <- as.character(data) #convert elements in the column to characters
  data <- substr(data,1,nchar(data)-1) #remove the last character for all elements in the column
  data <- as.numeric(data) #convert from character to numeric data
  return(data)
}
NY$Beech <- cleanUp(data=NY$Beech)
NY$Maples <- cleanUp(data=NY$Maples)
NY$Birches <- cleanUp(data=NY$Birches)
NY$Ashs <- cleanUp(data=NY$Ashs)
NY$Hemlock <- cleanUp(data=NY$Hemlock)
NY$Basswood <- cleanUp(data=NY$Basswood)
NY$Elms <- cleanUp(data=NY$Elms)
NY$Pines <- cleanUp(data=NY$Pines)
NY$Hickories <- cleanUp(data=NY$Hickories)
NY$Spruces <- cleanUp(data=NY$Spruces)
NY$Fir <- cleanUp(data=NY$Fir)
NY$Cedar <- cleanUp(data=NY$Cedar)
NY$Oaks <- cleanUp(data=NY$Oaks)
NY$Chestnut <- cleanUp(data=NY$Chestnut)
NY$Ironwoods <- cleanUp(data=NY$Ironwoods)
NY$Poplars <- cleanUp(data=NY$Poplars)
NY$Tamarack <- cleanUp(data=NY$Tamarack)
NY$Cherries <- cleanUp(data=NY$Cherries)
NY$Chamae.is <- cleanUp(data=NY$Chamae.is)
NY$Nyssa <- cleanUp(data=NY$Nyssa)
NY$Juglans <- cleanUp(data=NY$Juglans)
NY$Buttonwood <- cleanUp(data=NY$Buttonwood)
NY$Liriodendron <- cleanUp(data=NY$Liriodendron)
NY$Magnolia <- cleanUp(data=NY$Magnolia)

#rbind new england and new york datasets:
wit <- rbind(NE, NY)

wit2 <- lapply(wit[,7:30], as.numeric)

str(wit)

states <- map_data("state")#turn state line map into data frame
new_england <- subset(states, region %in% c("vermont", "new hampshire", "connecticut", "maine", "rhode island", "massachusetts", "new york"))#subest new england states

#points are the center most point of the town, plotted over map of new england:
ggplot() + 
  geom_polygon(data = new_england, aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_point(data=wit, aes(x=Long.,y=Lat.)) +
  ggtitle("Witness Tree Dataset: center point of 910 towns in the Northeastern United States")


#--------------------------------------------------------------------------------------------------
#--------------Species Accumulatio Curve--------------------------------------------------------


#Some of this code was taken from my final project for advanced computation in life science with Elizabeth Adair:

#     "This analysis aims to fit a rarefaction curve to the witness tree dataset. Rarefaction curves, or species accumulation curves, relate species richness to sample size (Fisher, R. A. 1943, Ugland, K. I. 2003). As the number of individuals you sample from a community increases, so does the likelihood of sampling a new species. Because richness is finite, the rarefaction curve eventually flattens, indicating that no additional sampling will yield a new species. Rarefaction curves are often used to estimate species richness in hyperdiverse clades or communities not easily accessible (Colwell, R. K. 1994). In a similar way, access to historic community richness data is limited. This curve could be used to identify sample points (towns) in the witness tree dataset that offer a true representation of forest species composition at the time of settlement. For example, data points that do not meet the minimum sample size needed to yield maximum richness (x value at which the curve flattens) will not be compared with the LPJ-GUESS output."

  
  #Witness trees: used by surveyors at the time of european settlement to mark property boundaries. Charlie Cogbill has compiled these records from towns across New England (Cogbill, C.V. 2002). This dataset will be compared with the model output for composition approximately 350 years ago (dates of surveys vary). 

#the r markdown file that shows the models I fit to the data should be in the github repository, 'cuden_NR395_finalPaper.Rmd. I won't include any of the models in this sript, but a spatial visualisation of richness and samples size is a good place to start when exploring this dataset:

#Ccalculate richness for each data point and plot against sample size. Sample size was log transformed in part to fit a linear model, but also because sample size varies so much -from 46 to 4477 trees. 

#add column of tallies for species richness
library(expss)

wit  <- modify(wit, {
  richness = count_row_if(gt(0), wit[7:30])
})
head(wit)
rich_base <- ggplot(wit, aes(x=Trees, y=richness)) +
  geom_point() +
  theme_bw(base_size=14) +
  xlab("Sample Size") +
  ylab("Species Richness")

#log transform Trees (sample size)
wit <- mutate(wit, logTrees=log(Trees))

rich_log_base <- ggplot(wit, aes(x=logTrees, y=richness)) +
  geom_point() +
  theme_bw(base_size=14) +
  xlab("log Sample Size") +
  ylab("Species Richness")

#Plot richness vs sample size: 
rich_base  
rich_log_base 

#----------------------State Boundary Map------------------------------------

states <- map_data("state")#turn state line map into data frame
northeast <- subset(states, region %in% c("vermont", "new hampshire", "connecticut", "maine", "rhode island", "massachusetts", "new york"))#subest new england states

#map with states and lpj-guess output: cmass

map <- ggplot() + 
  geom_polygon(data = northeast, aes(x=long, y = lat, group = group), fill='gray60', color='white') +
  coord_fixed(1.3) 
#----------------------------------------------------------------------------



map_richness <- map + 
  geom_point(wit, mapping=aes(x=Long., y=Lat., color=richness), size=4, alpha=0.9) +scale_color_gradient(low="yellow",high="blue") +
  ggtitle("Witness Tree Data: Number of Species Sampled")

map_sample <- map +
  geom_point(wit, mapping=aes(x=Long., y=Lat., color=logTrees), size=4, alpha=0.9) + 
  scale_color_gradient(low="yellow",high="blue") +
  ggtitle("Witness Tree Data: Sampel Size")

#Map richness and sample size: 
map_richness 
map_sample 

#These maps of species richness and sample size indicate that both decrease with latitued. So wether richness is a fucniotn of sample size is a tricky thing to get at with this dataset. But, if you want to have a look at the models I fit to the data, check out the r markdown file, cuden_NR395_finalPaper.Rmd. 


#--------------------------------------------------------------------------------------------------
#--------------Mapping species composition--------------------------------------------------------

#subset for columns I'm interested in:

wit2 <- select(wit,1:24)

#need to make a color palette that reflects global PFT's: 
  #green: BINE :boreal needle leaved everfreen shade intolerant
  #blue: BNE: boreal needleleaf evergreen shade tolerant 
  #yellow: TeBS: temperate broadleaf summergreen shade tolerant
  #red: IBS: boreal/temperate broadleaf summergreen shade intolerant

chiz <- c("#ef8834", #orange
          "#efb734", #mustard
          "#E4F316", #lime green
          "#F31616", #red
          "#3AF9AD", #mint
          "#F5ADE6", #pink
          "#F2936A", #peah
          "#D85E00", #dark orange
          "#CCE5FF", #babu blue
          "#00FFFF", #cayanne (blue)
          "#F5AA3A", #beige-orange
          "#FFEF00", #sticky note yellow
          "#DAA520", #goldenrod
          "#FF6347", #tomato
          "#66CDAA", #aqya marine
          "#FF1493", #hot pink
          "#4169E1", #purply blue
          "#9ACD32") #yellow green
          
          
          
          
library(scatterpie) #make map of pie charts


genusComp <- map + 
  geom_scatterpie(aes(x=Long., y=Lat., r=0.08), data=wit2, cols=c("Beech", "Maples", "Birches", "Ashs", "Hemlock", "Basswood", "Elms", "Pines", "Hickories", "Spruces", "Fir", "Cedar", "Oaks", "Chestnut", "Ironwoods", "Poplars", "Tamarack", "Cherries"), color=NA) +
  coord_fixed() + 
  scale_fill_manual(values=chiz) + 
  ggtitle("Witness Trees Genus Proportions")

genusComp


#---------------------convert genus columns to PFT's----------------------------------------------
#'BNE', 'BINE', 'TeBS', 'IBS',

#add columns 
wit <- mutate(wit, 
              BNE=Spruces+Hemlock+Fir+Tamarack, 
              BINE=Pines+Cedar,
              TeBS=Beech+Maples+Ashs+Basswood+Elms+Oaks+Chestnut+Ironwoods+Hickories+Cherries,
              IBS=Birches+Poplars)

PFTpalette <- c("#4169E1", #blue
                "#69A73B", #green
                "#FFEC66",#yellow
                "#FF6347") #red

witPFT <- map + 
  geom_scatterpie(aes(x=Long., y=Lat., r=0.08), data=wit, cols=c('BNE', 'BINE', 'TeBS', 'IBS'), color=NA) +
  coord_fixed() + 
  scale_fill_manual(values=PFTpalette) + 
  ggtitle("Witness Trees organsed by global PFT's")

witPFT






