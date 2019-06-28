#FIA data mart -open source. so need to figure out how to clump points. but need to go into the epscore building to accesss actual coordinates. 
#multiple dates -same point??

#column names in FIA data:

#CN -control number
#STATECD
#COUNTYCD
#PLOT
#INVYR
#LAT_ACTUAL-NAD83
#LON_ACTUAL_NAD83


nhTrees <- read.csv("/Users/charlotteuden/Desktop/trees/data/FIA/NH_TREE.csv")
vtTrees <- read.csv("/Users/charlotteuden/Desktop/trees/data/FIA/VT_TREE.csv")
head(vtTrees) #this is a massive data set (162354 rows and 207 columns), so i need to subset it. 
vtTrees2 <- c(vtTrees$CN, vtTrees$PLT_CN, vtTrees$INVYR, vtTrees$PLOT, vtTrees$TREE, vtTrees$SPCD, vtTrees$DIA, 

colnames(vtTrees)
head(vtTrees$SPCD)#species names is by number: 541=white ash, 531=american beech, etc. 

library(dplyr)
#### filter(); lets you pick/subset observations based on their values
## uses > >= < <= !=  ==
## logical operators & | !
ash <- filter(vtTrees, SPCD == 541 & ) 
filter(starwarsClean, eye_color %in% c("blue,","brown"))
