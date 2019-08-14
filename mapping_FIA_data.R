###Figuring out how to bind lat long data frame with FIA species data based on plot code 


#read in FIA data (open source)

vtTrees <- read.csv("/Users/charlotteuden/Desktop/trees/FIA_data/VT_TREE.csv")

colnames(vtTrees)

####################
# FUNCTION: FIACleanUp
# INPUTS: FIA dataset.
# OUTPUTS: subset for variables we are interested in, correctly named. SPCD (species ID) should be a factor.
FIACleanUp <- function(table=x){
  table2 <- data.frame(matrix(data=c(table$CN, table$PLT_CN, table$INVYR, table$PLOT, table$TREE, table$SPCD, table$DIA), ncol=7))
  names(table2) <- c("CT", "PLT_CN", "INVYR", "PLOT", "TREE", "SPCD", "DIA")
  #table2$SPCD <- as.factor(table2$SPCD) #make species ID a factor
  table2$PLOT <- as.factor(table2$PLOT) #same thing for plot number
  return(table2)
}
#------------------------------


vtTrees2 <- FIACleanUp(table=vtTrees)


#make a fake version of what liz has in her office. 

CN <- data.frame(PLT_CN=vtTrees2$PLT_CN, lat=runif(n=162354, min=-80, max=-66), long=runif(n=162354, min=40, max=48))

CN2 <- slice(CN, sample(1:n())) #shuffle rows, to see if the merge() funciton can match rows by PLT_CN

df <- merge(CN2, vtTrees2, by=intersect(names(CN2), names(vtTrees2)), sort=TRUE) 
head(df)
