

#---
 #New England Forest Composition at the time of European Settlement
#Charlotte Uden
#5/10/2019
#---
  
wit <- read.csv("witness tree /583NewEngland%.csv")
library(mapdata)
library(maps)
library(ggplot2)
library(dplyr)

states <- map_data("state")#turn state line map into data frame
new_england <- subset(states, region %in% c("vermont", "new hampshire", "connecticut", "maine", "rhode island", "massachusetts"))#subest new england states

#points are the center most point of the town, plotted over map of new england:
ggplot() + 
  geom_polygon(data = new_england, aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_point(data=wit, aes(x=Long.,y=Lat.))


#--------------------------------------------------------------------------------------------------
#--------------Species Accumulatio Curve--------------------------------------------------------


#Some of this code was taken from my final project for advanced computation in life science with Elizabeth Adair:

#     "This analysis aims to fit a rarefaction curve to the witness tree dataset. Rarefaction curves, or species accumulation curves, relate species richness to sample size (Fisher, R. A. 1943, Ugland, K. I. 2003). As the number of individuals you sample from a community increases, so does the likelihood of sampling a new species. Because richness is finite, the rarefaction curve eventually flattens, indicating that no additional sampling will yield a new species. Rarefaction curves are often used to estimate species richness in hyperdiverse clades or communities not easily accessible (Colwell, R. K. 1994). In a similar way, access to historic community richness data is limited. This curve could be used to identify sample points (towns) in the witness tree dataset that offer a true representation of forest species composition at the time of settlement. For example, data points that do not meet the minimum sample size needed to yield maximum richness (x value at which the curve flattens) will not be compared with the LPJ-GUESS output."

  
  #Witness trees: used by surveyors at the time of european settlement to mark property boundaries. Charlie Cogbill has compiled these records from towns across New England (Cogbill, C.V. 2002). This dataset will be compared with the model output for composition approximately 350 years ago (dates of surveys vary). 

#the r markdown file that shows the models I fit to the data should be in the github repository, 'cuden_NR395_finalPaper.Rmd. I won't include any of the models in this sript, but a spatial visualisation of richness and samples size is a good place to start when exploring this dataset:

#Ccalculate richness for each data point and plot against sample size. Sample size was log transformed in part to fit a linear model, but also because sample size varies so much -from 46 to 4477 trees. 

#add column of tallies for species richness
library(expss)
library(patchwork)
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
rich_base + rich_log_base 


map_richness <- ggplot(wit, mapping=aes(x=Long., y=Lat., color=richness)) +
  geom_point(size=4, alpha=0.9) + 
  ggtitle("Richness") +
  scale_color_gradient(low="yellow",high="blue")

map_sample <- ggplot(wit, mapping=aes(x=Long., y=Lat., color=logTrees)) +
  geom_point(size=4, alpha=0.9) + 
  ggtitle("Sample Size") +
  scale_color_gradient(low="yellow",high="blue")

#Map richness and sample size: 
map_richness + map_sample 

#These maps of species richness and sample size indicate that both decrease with latitued. So wether richness is a fucniotn of sample size is a tricky thing to get at with this dataset. But, if you want to have a look at the models I fit to the data, check out the r markdown file, cuden_NR395_finalPaper.Rmd. 


#--------------------------------------------------------------------------------------------------
#--------------Mapping species composition--------------------------------------------------------







#--------------------------------------------------------------------------------------------------
#--------------Adding time to the dataset--------------------------------------------------------
