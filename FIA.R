#---
#Forest Inventory and Analysis (FIA) data
#Charlotte Uden
#6/27/2019
#---

library(ggplot2) #for plotting
library(dplyr) #changing data structure
library(plyr)#use aggreggate() funciton for changing data format
library(reshape2)#
library(tidyr)


#The FIA dataset is open source. But if I want to get a spatial representation of the data, I have to build the code and then match the 'CN' number to the coordinate point. 

vtTrees <- read.csv("/Users/charlotteuden/Desktop/trees/FIA_data/VT_TREE.csv")

colnames(vtTrees)

#This is a massive data set (162354 rows and 207 columns), so I need to subset for the stuff i think I'll actually use. This may change as I decide what I want my output for LPJ-GUESS to be, but for now I'll choose the serial number (CN and PLOT_CN), year (INVYR), plot number (PLOT), tree number (TREE), species (SPCD) and diameter (DIA). 

#FIA data mart: https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html. Choose file nawm 'STATE_TREE.csv'

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

vtTrees4 <- speciesNamesFIA(table=vtTrees3)
head(vtTrees4)


#So I want to plot time by species. But this is going to be tricky because the dataset has years with speceis repeated. I want a cumulative quantity for each species for each year. I'm just going to sum basal area (BA) for now. But I can change this if there's a different variable we want to look at. 


library(reshape2)
library(tidyr)
library(dplyr)

####################
# FUNCTION: NAremove
# INPUTS: FIA dataset.
# OUTPUTS: remove rows that contain NA values
NAremove <- function(table=x){
  table <- table[complete.cases(table[8]),]
  return(table)
}
#------------------------------

anyNA(vtTrees4)#find out if therea are any NA values. you have to do this BEFORE you aggregate
vtTrees5 <- NAremove(table=vtTrees4)
anyNA(vtTrees5)#no NA'ss!

####################
# FUNCTION: sumBA
# INPUTS: FIA dataset.
# OUTPUTS: use aggregate() to find the sum basal area for each species for each year
sumBA <- function(table=x){
  table <- with(table, aggregate(BA, by = list(INVYR, SPCD), 
                                    FUN = "sum")) 
  names(table) <- c("INVYR", "SPCD", "sum.BA") #rename columns
  return(table)
}
#------------------------------

vtTrees6 <- sumBA(table=vtTrees5)

####################
# FUNCTION: filterSpecies
# INPUTS: FIA dataset.
# OUTPUTS: use filter() to select rows that have trees found in new england
filterSpecies <- function(table=x){
  table <- filter(table, SPCD %in% c("americanBasswood", "americanElm", "balsamFir", "blackAsh", "blackCherry", "blackSpruce", "burOak", "easternHemlock", "easternHophornbeam", "northernRedOak","northernWhiteCedar", "paperBirch", "balsamPoplar", "redMaple", "redPine", "sugarMaple", "whiteOak", "whitePine", "whiteSpruce", "yellowBirch", "whiteAsh", "redSpruce"))
  return(table)
}
#------------------------------

vtTrees7 <- filterSpecies(table=vtTrees6)

#color palett for my plot
chiz <- c("#ef8834","#efb734", "#2366d1", "#05723e", "#069b7d","#cc2d0a", "#4f6496", "#99501d", "#bbeaed", "#f7c640", "#8cdcea", "#8c991d", 
          "#651bf8", #purple
          "#ccf81b", #lime green
          "#f86c1b", #orange
          "#1bc1f8", #lighter blue
          "#ef95da", #light pink
          "#6ceaa0", #mint
          "#133d86", #dark blue
          "#862113", #rusty red
          "#e6a014", #mustard
          "#487231") #darker green

VTTreesTimeLine <- ggplot(vtTrees7, aes(x=INVYR, y=sum.BA, col=SPCD)) +
  geom_line(size=1) +
  scale_color_manual(values=chiz) +
  ggtitle("FIA dataset: Total Basal Area by Species, VT")

VTTreesTimeLine

#rather than total basal area for each species, we may want proportion of basal area (witness tree dataset is proportion of a tally):

library(plyr)

####################
# FUNCTION: proportionSpecies
# INPUTS: FIA dataset.
# OUTPUTS: make a column of species proportion of basal area
proportionSpecies <- function(table=x){
  table <- ddply(table,.(INVYR),transform,prop=sum.BA/sum(sum.BA))
  return(table)
}
#------------------------------


vtTrees8 <- proportionSpecies(table=vtTrees7)


VTspecesArea <- ggplot(vtTrees8, aes(x=INVYR, y=prop, fill=SPCD)) +
  geom_area(alpha=1) +
  scale_fill_manual(values=chiz) +
  ggtitle("FIA dataset: Proportion of Basal Area by Species, VT") +
  ylab("Proportion of basal area")

VTspecesArea #pretty, but doesn't show increas/decrease of individual species very well. 

VTspecesLine <- ggplot(vtTrees8, aes(x=INVYR, y=prop, col=SPCD)) +
  geom_line(size=1) +
  scale_color_manual(values=chiz) +
  ggtitle("FIA dataset: Proportion of Basal Area by Species, VT") +
  ylab("Proportion of basal area")

VTspecesLine

#VTsumSpeciesLine <- ggplot(vtTrees8, aes(x=INVYR, y=sum.BA, col=SPCD)) +
  #geom_line(size=1) +
  #scale_color_manual(values=chiz) +
  #ggtitle("FIA dataset: Sum of Basal Area by Species, VT")

#VTsumSpeciesLine #because some plots are not re-visited, summing data makes it look like basal area devreased -which maybe it did (development etc.) but we are already comparing proportions for ht eFIA dataset and the witness tree data set. so a sum makes less sense. 


#---------------------sort species into global PFT's----------------------------------------

####################
# FUNCTION: sortPFT
# INPUTS: FIA dataset. must have columnes: INVYR, SPCD, prop
# OUTPUTS: sort species columns into LPJ-GUEss global PFT's
sortPFT <- function(table=x){
  #Only want to plot speceis proportions for each year
  table <- select(table, INVYR, SPCD, prop)
  #Convert data to wide format, so that there is a column for each species. a row for each year
  table <- reshape(table, idvar = "INVYR", timevar = "SPCD", direction = "wide")
  #now dataset has NA values. #replace NA's with 0
  table[is.na(table)] <- 0
  #add columns that clump species by global PFT:
  table <- mutate(table, 
                     BNE=prop.balsamFir+prop.blackSpruce+prop.easternHemlock+prop.redSpruce+prop.whiteSpruce, 
                     BINE=prop.northernWhiteCedar+prop.redPine+prop.whitePine,
                     TeBS=prop.americanBasswood+prop.americanElm+prop.blackAsh+prop.burOak+prop.northernRedOak+prop.easternHophornbeam+prop.redMaple+prop.sugarMaple+prop.whiteAsh+prop.whiteOak,
                     IBS=prop.balsamPoplar+prop.paperBirch+prop.yellowBirch)
  #convert data back to long format, so that there area three columns: INVY (inventory year), variabl (global PFT), value (sum of basal area for that pft in that year)
  table <- melt(data = table, id.vars = "INVYR", measure.vars = c('BNE',  'BINE', 'TeBS', 'IBS'))
  return(table)
}
#------------------------------


vtTrees9 <- sortPFT(table=vtTrees8)
head(vtTrees9)

PFTpalette <- c("#4169E1", #blue
                "#69A73B", #green
                "#FFEC66", #yellow
                "#FF6347") #red

VTpftLine <- ggplot() + geom_line(vtTrees9, mapping=aes(x=INVYR, y=value, col=variable), size=1) +
  scale_color_manual(values=PFTpalette) +
  ggtitle("FIA dataset, Vermont: total basal area by PFT") +
  ylab("Proportion of basal area") + xlab("year")

VTpftLine



#-----------------------------------------------------------------------------------------------
#--------------------------------New Hampshire-------------------------------------------------
#-----------------------------------------------------------------------------------------------


nhTrees <- read.csv("/Users/charlotteuden/Desktop/trees/FIA_data/NH_TREE.csv")

nhTrees1 <- FIACleanUp(table=nhTrees) #select columns you want
nhTrees2 <- calcBA(table=nhTrees1) #calculate basal area
nhTrees3 <- speciesNamesFIA(table=nhTrees2) #change species codes with species common name
nhTrees4 <- NAremove(table=nhTrees3) #remove NA values before finding sum of basal areas
nhTrees5 <- sumBA(table=nhTrees4) #Sum basal area for each species for each year
nhTrees6 <- filterSpecies(table=nhTrees5) #only work with speceis found in new england
nhTrees7 <- proportionSpecies(table=nhTrees6) #calculate proportion of basal area

sortPFT2 <- function(table=x){
  #Only want to plot speceis proportions for each year
  table <- select(table, INVYR, SPCD, prop)
  #Convert data to wide format, so that there is a column for each species. a row for each year
  table <- reshape(table, idvar = "INVYR", timevar = "SPCD", direction = "wide")
  #now dataset has NA values. #replace NA's with 0
  table[is.na(table)] <- 0
  #add columns that clump species by global PFT:
  table <- mutate(table, 
                  BNE=prop.balsamFir+prop.blackSpruce+prop.easternHemlock+prop.redSpruce+prop.whiteSpruce, 
                  BINE=prop.northernWhiteCedar+prop.redPine+prop.whitePine,
                  TeBS=prop.americanBasswood+prop.americanElm+prop.blackAsh+prop.northernRedOak+prop.easternHophornbeam+prop.redMaple+prop.sugarMaple+prop.whiteAsh+prop.whiteOak,
                  IBS=prop.balsamPoplar+prop.paperBirch+prop.yellowBirch)
  #convert data back to long format, so that there area three columns: INVY (inventory year), variabl (global PFT), value (sum of basal area for that pft in that year)
  table <- melt(data = table, id.vars = "INVYR", measure.vars = c('BNE',  'BINE', 'TeBS', 'IBS'))
  return(table)
} #New hampshire dataset has no bur oak, so need to make a slightly different function for NH that doesn not include bur oak 

nhTrees8 <- sortPFT2(table=nhTrees7) #Sort speceis into PFT's: three column table of INVYR, PFT, prop



NHspeceisLine <- ggplot(nhTrees7, aes(x=INVYR, y=prop, col=SPCD)) +
  geom_line(size=1) +
  scale_color_manual(values=chiz) +
  ggtitle("FIA dataset: Proportion of Basal Area by Species, New Hampshire") +
  ylab("Proportion of basal area")

NHspeceisLine 

NHpftLine <- ggplot() + geom_line(nhTrees8, mapping=aes(x=INVYR, y=value, col=variable), size=1) +
  scale_color_manual(values=PFTpalette) +
  ggtitle("FIA dataset, New Hampshire: total basal area by PFT") +
  ylab("Proportion of basal area") + xlab("year")

NHpftLine


#-----------------------------------------------------------------------------------------------
#--------------------------------Maine-------------------------------------------------
#-----------------------------------------------------------------------------------------------


meTrees <- read.csv("/Users/charlotteuden/Desktop/trees/FIA_data/ME_TREE.csv")

meTrees <- FIACleanUp(table=meTrees) #select columns you want
meTrees <- calcBA(table=meTrees) #calculate basal area
meTrees <- speciesNamesFIA(table=meTrees) #change species codes with species common name
meTrees <- NAremove(table=meTrees) #remove NA values before finding sum of basal areas
meTrees <- sumBA(table=meTrees) #Sum basal area for each species for each year
meTrees <- filterSpecies(table=meTrees) #only work with speceis found in new england
meTrees <- proportionSpecies(table=meTrees) #calculate proportion of basal area
meTrees2 <- sortPFT(table=meTrees) #Sort speceis into PFT's: three column table of INVYR, PFT, prop



MEspeceisLine <- ggplot(meTrees, aes(x=INVYR, y=prop, col=SPCD)) +
  geom_line(size=1) +
  scale_color_manual(values=chiz) +
  ggtitle("FIA dataset: Proportion of Basal Area by Species, Maine") +
  ylab("Proportion of basal area")

MEspeceisLine 

MEpftLine <- ggplot() + geom_line(meTrees2, mapping=aes(x=INVYR, y=value, col=variable), size=1) +
  scale_color_manual(values=PFTpalette) +
  ggtitle("FIA dataset, Maine: total basal area by PFT") +
  ylab("Proportion of basal area") + xlab("year")

MEpftLine


#-----------------------------------------------------------------------------------------------
#--------------------------------Massachusettes-------------------------------------------------
#-----------------------------------------------------------------------------------------------


maTrees <- read.csv("/Users/charlotteuden/Desktop/trees/FIA_data/MA_TREE.csv")

maTrees <- FIACleanUp(table=maTrees) #select columns you want
maTrees <- calcBA(table=maTrees) #calculate basal area
maTrees <- speciesNamesFIA(table=maTrees) #change species codes with species common name
maTrees <- NAremove(table=maTrees) #remove NA values before finding sum of basal areas
maTrees <- sumBA(table=maTrees) #Sum basal area for each species for each year
maTrees <- filterSpecies(table=maTrees) #only work with speceis found in new england
maTrees <- proportionSpecies(table=maTrees) #calculate proportion of basal area
maTrees2 <- sortPFT(table=maTrees) #Sort speceis into PFT's: three column table of INVYR, PFT, prop



MAspeceisLine <- ggplot(maTrees, aes(x=INVYR, y=prop, col=SPCD)) +
  geom_line(size=1) +
  scale_color_manual(values=chiz) +
  ggtitle("FIA dataset: Proportion of Basal Area by Species, Massachusettes") +
  ylab("Proportion of basal area")

MAspeceisLine 

MApftLine <- ggplot() + geom_line(maTrees2, mapping=aes(x=INVYR, y=value, col=variable), size=1) +
  scale_color_manual(values=PFTpalette) +
  ggtitle("FIA dataset, Massachusettes: total basal area by PFT") +
  ylab("Proportion of basal area") + xlab("year")

MApftLine


#-----------------------------------------------------------------------------------------------
#--------------------------------Connecticute-------------------------------------------------
#-----------------------------------------------------------------------------------------------


ctTrees <- read.csv("/Users/charlotteuden/Desktop/trees/FIA_data/CT_TREE.csv")

ctTrees <- FIACleanUp(table=ctTrees) #select columns you want
ctTrees <- calcBA(table=ctTrees) #calculate basal area
ctTrees <- speciesNamesFIA(table=ctTrees) #change species codes with species common name
ctTrees <- NAremove(table=ctTrees) #remove NA values before finding sum of basal areas
ctTrees <- sumBA(table=ctTrees) #Sum basal area for each species for each year
ctTrees <- filterSpecies(table=ctTrees) #only work with speceis found in new england
ctTrees <- proportionSpecies(table=ctTrees) #calculate proportion of basal area

sortPFT3 <- function(table=x){
  #Only want to plot speceis proportions for each year
  table <- select(table, INVYR, SPCD, prop)
  #Convert data to wide format, so that there is a column for each species. a row for each year
  table <- reshape(table, idvar = "INVYR", timevar = "SPCD", direction = "wide")
  #now dataset has NA values. #replace NA's with 0
  table[is.na(table)] <- 0
  #add columns that clump species by global PFT:
  table <- mutate(table, 
                  BNE=prop.balsamFir+prop.easternHemlock+prop.redSpruce, 
                  BINE=prop.northernWhiteCedar+prop.redPine+prop.whitePine,
                  TeBS=prop.americanBasswood+prop.americanElm+prop.blackAsh+prop.northernRedOak+prop.easternHophornbeam+prop.redMaple+prop.sugarMaple+prop.whiteAsh+prop.whiteOak,
                  IBS=prop.paperBirch+prop.yellowBirch)
  #convert data back to long format, so that there area three columns: INVY (inventory year), variabl (global PFT), value (sum of basal area for that pft in that year)
  table <- melt(data = table, id.vars = "INVYR", measure.vars = c('BNE',  'BINE', 'TeBS', 'IBS'))
  return(table)
} #Connecticut dataset has no white spruce, black spruce, bur oak or balsam poplar, so need to make a slightly different function for CT that doesn not include black spruce

ctTrees2 <- sortPFT3(table=ctTrees) #Sort speceis into PFT's: three column table of INVYR, PFT, prop



CTspeceisLine <- ggplot(ctTrees, aes(x=INVYR, y=prop, col=SPCD)) +
  geom_line(size=1) +
  scale_color_manual(values=chiz) +
  ggtitle("FIA dataset: Proportion of Basal Area by Species, Connecticut") +
  ylab("Proportion of basal area")

CTspeceisLine 

CTpftLine <- ggplot() + geom_line(ctTrees2, mapping=aes(x=INVYR, y=value, col=variable), size=1) +
  scale_color_manual(values=PFTpalette) +
  ggtitle("FIA dataset, Connecticut: total basal area by PFT") +
  ylab("Proportion of basal area") + xlab("year")

CTpftLine


#-----------------------------------------------------------------------------------------------
#--------------------------------Rhode Island-------------------------------------------------
#-----------------------------------------------------------------------------------------------


riTrees <- read.csv("/Users/charlotteuden/Desktop/trees/FIA_data/RI_TREE.csv")

riTrees <- FIACleanUp(table=riTrees) #select columns you want
riTrees <- calcBA(table=riTrees) #calculate basal area
riTrees <- speciesNamesFIA(table=riTrees) #change species codes with species common name
riTrees <- NAremove(table=riTrees) #remove NA values before finding sum of basal areas
riTrees <- sumBA(table=riTrees) #Sum basal area for each species for each year
riTrees <- filterSpecies(table=riTrees) #only work with speceis found in new england
riTrees <- proportionSpecies(table=riTrees) #calculate proportion of basal area

sortPFT4 <- function(table=x){
  #Only want to plot speceis proportions for each year
  table <- select(table, INVYR, SPCD, prop)
  #Convert data to wide format, so that there is a column for each species. a row for each year
  table <- reshape(table, idvar = "INVYR", timevar = "SPCD", direction = "wide")
  #now dataset has NA values. #replace NA's with 0
  table[is.na(table)] <- 0
  #add columns that clump species by global PFT:
  table <- mutate(table, 
                  BNE=prop.balsamFir+prop.easternHemlock, 
                  BINE=prop.redPine+prop.whitePine,
                  TeBS=prop.americanElm+prop.blackAsh+prop.northernRedOak+prop.easternHophornbeam+prop.redMaple+prop.sugarMaple+prop.whiteAsh+prop.whiteOak,
                  IBS=prop.balsamPoplar+prop.paperBirch+prop.yellowBirch)
  #convert data back to long format, so that there area three columns: INVY (inventory year), variabl (global PFT), value (sum of basal area for that pft in that year)
  table <- melt(data = table, id.vars = "INVYR", measure.vars = c('BNE',  'BINE', 'TeBS', 'IBS'))
  return(table)
} #Rhode island has no red pruce, black spruce, white spruce, white cedar, amoerican basswood or bur oak. so re-wrote function to disclude these species. 

riTrees2 <- sortPFT4(table=riTrees) #Sort speceis into PFT's: three column table of INVYR, PFT, prop



RIspeceisLine <- ggplot(riTrees, aes(x=INVYR, y=prop, col=SPCD)) +
  geom_line(size=1) +
  scale_color_manual(values=chiz) +
  ggtitle("FIA dataset: Proportion of Basal Area by Species, Rhode Island")

RIspeceisLine 

RIpftLine <- ggplot() + geom_line(riTrees2, mapping=aes(x=INVYR, y=value, col=variable), size=1) +
  scale_color_manual(values=PFTpalette) +
  ggtitle("FIA dataset, Rhode Island: total basal area by PFT") +
  ylab("Proportion of basal area") + xlab("year")

RIpftLine


#-----------------------------------------------------------------------------------------------
#--------------------------------New York-------------------------------------------------
#-----------------------------------------------------------------------------------------------


nyTrees <- read.csv("/Users/charlotteuden/Desktop/trees/FIA_data/NY_TREE.csv")

nyTrees <- FIACleanUp(table=nyTrees) #select columns you want
nyTrees <- calcBA(table=nyTrees) #calculate basal area
nyTrees <- speciesNamesFIA(table=nyTrees) #change species codes with species common name
nyTrees <- NAremove(table=nyTrees) #remove NA values before finding sum of basal areas
nyTrees <- sumBA(table=nyTrees) #Sum basal area for each species for each year
nyTrees <- filterSpecies(table=nyTrees) #only work with speceis found in new england
nyTrees <- proportionSpecies(table=nyTrees) #calculate proportion of basal area
nyTrees2 <- sortPFT(table=nyTrees) #Sort speceis into PFT's: three column table of INVYR, PFT, prop



NYspeceisLine <- ggplot(nyTrees, aes(x=INVYR, y=prop, col=SPCD)) +
  geom_line(size=1) +
  scale_color_manual(values=chiz) +
  ggtitle("FIA dataset: Proportion of Basal Area by Species, New York") + 
  ylab("Proportion of basal area")

NYspeceisLine 

NYpftLine <- ggplot() + geom_line(nyTrees2, mapping=aes(x=INVYR, y=value, col=variable), size=1) +
  scale_color_manual(values=PFTpalette) +
  ggtitle("FIA dataset, New York: total basal area by PFT") +
  ylab("Proportion of basal area") + xlab("year")

NYpftLine





VTspeceisLine
VTpftLine

NHspeceisLine
NHpftLine

MEspeceisLine
MEpftLine

RIspeceisLine
RIpftLine

CTspeceisLine
CTpftLine

MAspeceisLine
MApftLine

NYspeceisLine
NYpftLine

#-------------------------------dataset of species composition for all states in 2017-------------------------------------


head(nhTrees5)
#remove columns that still have numeric values for species 



#use proportionSpecies() function I made to find proportion of basal area for each species 

nh2017 <- filter(nhTrees6, INVYR==2017)
proportionSpecies(table=nh2017)
