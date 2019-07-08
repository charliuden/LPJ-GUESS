#---
#Forest Inventory and Analysis (FIA) data
#Charlotte Uden
#6/27/2019
#---


#The FIA dataset is open source. But if I want to get a spatial representation of the data, I have to build the code and then match the 'CN' number to the coordinate point. 



nhTrees <- read.csv("FIA/NH_TREE.csv")
vtTrees <- read.csv("FIA/VT_TREE.csv")

colnames(vtTrees)

#This is a massive data set (162354 rows and 207 columns), so I need to subset for the stuff i think I'll actually use. This may change as I decide what I want my output for LPJ-GUESS to be, but for now I'll choose the serial number (CN and PLOT_CN), year (INVYR), plot number (PLOT), tree number (TREE), species (SPCD) and diameter (DIA). 

#For information on what each column is and the codes within each column (ie. species names is by number: 541=white ash, 531=american beech), go to:
  
  #https://www.fia.fs.fed.us/library/database-documentation/index.php



####################
# FUNCTION: FIACleanUp
# INPUTS: FIA dataset.
# OUTPUTS: subset for variables we are interested in, correctly named. SPCD (species ID) should be a factor.
FIACleanUp <- function(table=x){
  table2 <- data.frame(matrix(data=c(table$CN, table$PLT_CN, table$INVYR, table$PLOT, table$TREE, table$SPCD, table$DIA), ncol=7))
  names(table2) <- c("CT", "PLT_CN", "INVYR", "PLOT", "TREE", "SPCD", "DIA")
  #table2$SPCD <- as.factor(table2$SPCD) #make species ID a factor
  table2$PLOT <- as.factor(table2$PLOT) #same thin gfor plot number
  return(table2)
}
#------------------------------


vtTrees2 <- FIACleanUp(table=vtTrees)
head(vtTrees2)
str(vtTrees2)


#Build funciton to calculate basal area: BA = 0.005454 * DBH^2. Basal area is in square feet. 


library(dplyr)
####################
# FUNCTION: calcBA
# INPUTS: FIA dataset.
# OUTPUTS: calculate basa area from diameter. uses package dplyr
calcBA <- function(table=x){
  table2 <- mutate(table, BA = 0.005454 * DIA^2)
  return(table2)
}
#------------------------------

vtTrees3 <- calcBA(table=vtTrees2)


#Species are also recorded as numbers. I need to build a funciton to replace these numbers with names, so the data makes more sense.



vtTrees4 <- select(vtTrees3, INVYR, SPCD, BA) #subset for columns I want

####################
# FUNCTION: speciesNamesFIA()
# INPUTS: FIA dataset.
# OUTPUTS: table with numeric values for species replaced with species common names. 

speciesNamesFIA <- function(table=x){
  table$SPCD[table$SPCD==12] <- "balsamFir"
  table$SPCD[table$SPCD==43] <- "whiteCedar"
  table$SPCD[table$SPCD==68] <- "redCedar"
  table$SPCD[table$SPCD==70] <- "larch"
  table$SPCD[table$SPCD==71] <- "tamarack"
  table$SPCD[table$SPCD==91] <- "norwaySpruce"
  table$SPCD[table$SPCD==94] <- "whiteSpruce"
  table$SPCD[table$SPCD==95] <- "blackSpruce"
  table$SPCD[table$SPCD==96] <- "blueSpruce"
  table$SPCD[table$SPCD==97] <- "redSpruce"
  table$SPCD[table$SPCD==125] <- "redPine"
  table$SPCD[table$SPCD==129] <- "whitePine"
  table$SPCD[table$SPCD==130] <- "scotchPine"
  table$SPCD[table$SPCD==241] <- "northernWhiteCedar"
  table$SPCD[table$SPCD==261] <- "easternHemlock"
  table$SPCD[table$SPCD==299] <- "Unk.deadConifer"
  table$SPCD[table$SPCD==310] <- "maple"
  table$SPCD[table$SPCD==313] <- "boxelder"
  table$SPCD[table$SPCD==315] <- "stripedMaple"
  table$SPCD[table$SPCD==316] <- "redMaple"
  table$SPCD[table$SPCD==317] <- "silverMaple"
  table$SPCD[table$SPCD==318] <- "sugarMaple"
  table$SPCD[table$SPCD==319] <- "mountainMaple"
  table$SPCD[table$SPCD==341] <- "ailanthus"
  table$SPCD[table$SPCD==355] <- "europeanAlder"
  table$SPCD[table$SPCD==356] <- "serviceberry"
  table$SPCD[table$SPCD==357] <- "commoneServiceberry"
  table$SPCD[table$SPCD==371] <- "yellowBirch"
  table$SPCD[table$SPCD==372] <- "sweetBirch"
  table$SPCD[table$SPCD==375] <- "paperBirch"
  table$SPCD[table$SPCD==379] <- "grayBirch"
  table$SPCD[table$SPCD==391] <- "musclewood"
  table$SPCD[table$SPCD==400] <- "hickory"
  table$SPCD[table$SPCD==402] <- "bitternutHickory"
  table$SPCD[table$SPCD==403] <- "pignutHickory"
  table$SPCD[table$SPCD==407] <- "shagbarkHickory"
  table$SPCD[table$SPCD==409] <- "mockernutHickory"
  table$SPCD[table$SPCD==500] <- "hawthorn"
  table$SPCD[table$SPCD==531] <- "americanBeech"
  table$SPCD[table$SPCD==540] <- "ash"
  table$SPCD[table$SPCD==541] <- "whiteAsh"
  table$SPCD[table$SPCD==543] <- "blackAsh"
  table$SPCD[table$SPCD==544] <- "greenAsh"
  table$SPCD[table$SPCD==552] <- "honeylocust"
  table$SPCD[table$SPCD==601] <- "butternut"
  table$SPCD[table$SPCD==602] <- "blackWalnut"
  table$SPCD[table$SPCD==660] <- "apple"
  table$SPCD[table$SPCD==680] <- "mulberry"
  table$SPCD[table$SPCD==701] <- "easternHophornbeam"
  table$SPCD[table$SPCD==741] <- "balsamPoplar"
  table$SPCD[table$SPCD==742] <- "easternCottonwood"
  table$SPCD[table$SPCD==743] <- "bigtoothAspen"
  table$SPCD[table$SPCD==746] <- "quakingAspen"
  table$SPCD[table$SPCD==761] <- "pinCherry"
  table$SPCD[table$SPCD==762] <- "blackCherry"
  table$SPCD[table$SPCD==763] <- "chockecherry"
  table$SPCD[table$SPCD==771] <- "sweetCherry"
  table$SPCD[table$SPCD==800] <- "oak"
  table$SPCD[table$SPCD==802] <- "whiteOak"
  table$SPCD[table$SPCD==804] <- "swampWhiteOak"
  table$SPCD[table$SPCD==823] <- "burOak"
  table$SPCD[table$SPCD==832] <- "chestnutOak"
  table$SPCD[table$SPCD==833] <- "northernRedOak"
  table$SPCD[table$SPCD==837] <- "blackOak"
  table$SPCD[table$SPCD==901] <- "blackLocust"
  table$SPCD[table$SPCD==920] <- "willow"
  table$SPCD[table$SPCD==922] <- "blackWillow"
  table$SPCD[table$SPCD==934] <- "mountainAsh"
  table$SPCD[table$SPCD==935] <- "americanMountainAsh"
  table$SPCD[table$SPCD==950] <- "basswood"
  table$SPCD[table$SPCD==951] <- "americanBasswood"
  table$SPCD[table$SPCD==972] <- "americanElm"
  table$SPCD[table$SPCD==975] <- "slipperyElm"
  table$SPCD[table$SPCD==977] <- "rockElm"
  table$SPCD[table$SPCD==998] <- "whiteMangrove"
  table$SPCD[table$SPCD==999] <- "americanMangrove"
  return(table)
}
#------------------------------

vtTrees4 <- speciesNamesFIA(table=vtTrees4)
head(vtTrees4)



#So I want to plot time by species. But this is going to be tricky because the dataset has years with speceis repeated. I want a cumulative quantity for each species for each year. I'm just going to sum basal area (BA) for now. But I can change this if there's a different variable we want to look at. 



library(reshape2)
library(tidyr)

anyNA(vtTrees4) #find out if therea are any NA values. you have to do this BEFORE you aggregate
vtTrees5 <-  vtTrees4[complete.cases(vtTrees4[3]),] #remove NA values in BA column 
anyNA(vtTrees5)#no NA'ss!

#use aggregate() to find the sum basa area for each species for each year
vtTrees6 <- with(vtTrees5, aggregate(BA, by = list(INVYR, SPCD), 
                                     FUN = "sum")) 

names(vtTrees6) <- c("INVYR", "SPCD", "sum.BA") #rename columns
head(vtTrees6)

library(ggplot2)
#temporal representation of PFT composition
vtTreesTime <- ggplot(vtTrees6, aes(x=INVYR, y=log(sum.BA), fill=SPCD)) +
  geom_area(alpha=1) +
  #scale_fill_manual(values=chiz) +
  ggtitle("Species Basal Area, VT")

vtTreesTime

#This is a terrible plot. A problem that really sticks out at this point is that there are a lot of speceis. It may be worth just looking at species I built PFT's for -major/more dominant over story speceis. 


vtTrees7 <- filter(vtTrees6, SPCD %in% c("americanBasswood", "americanElm", "balsamFir", "blackAsh", "blackCherry", "blackSpruce", "burOak", "easternHemlock", "easternHophornbeam", "northernRedOak","northernWhiteCedar", "paperBirch", "balsamPoplar", "redMaple", "redPine", "sugarMaple", "whiteOak", "whitePine", "whiteSpruce", "yellowBirch", "whiteAsh", "redSpruce"))

vtTreesTime2 <- ggplot(vtTrees7, aes(x=INVYR, y=log(sum.BA), fill=SPCD)) +
  geom_area(alpha=1) +
  #scale_fill_manual(values=chiz) +
  ggtitle("Species Basal Area, VT")

vtTreesTime2


#This plot would also make more sense if, rather than total basal area for each species, it was proportion of basal area. First I'll make a column of total basal area for each year. and then I can use that to calculate the proporiton for each species. 





