#---
#Exploring Fire in LPJ-GEUSS
#Charlotte Uden
#6/26/2019
#---


##------------Fire Resistance------------

#Hickler 2004 gives fire resistant trees (white oak, bur oak, northern red oak, white pine and red pine) a value of 0.04 for R. All non fire resistant trees: R=0.07


D <- 5 #tree diameter
R <- seq(0,1, by=0.01) #fire resistance shape parameter (0-1)
P <- 1-(1/(1+(D/R)^1.5)+0.05) #probability of survival

plot(R,P) #trees with a diameter of 5, varying resistance to fire

D <- seq(0,3, length=100) #tree diameter
RS <- 0.07 #fire resistance shape parameter (0-1)
RR <- 0.04
PR <- 1-(1/(1+(D/RR)^1.5)+0.05)
PS <- 1-(1/(1+(D/RS)^1.5)+0.05)
#probability of survival
plot(D,PR, type="l")
lines(D,PS, type="l")


#Once you get to a diameter of less than 1, surival is 1. This seems a little off... I find it hard to believe that a one inch tree, no matter how well adapted it is to fire, would survive. 

#After looking into this a little more, I found that the LPJ-GUESS manual and Thonicke 2001 both describe the proportion of individuals that die as R-1, where R is fire resistance. This makes a lot more sense. 


##------------Fire Frequency------------

#Thonicke 2001 gives a detailed description of fire frequency in LPJ-DGVM (early version of LPJ-GUES). The probability of fire occurrence increases with time since last fire (as fuel builds up). Litter moisture is the main driver for day to day fire probability and fire season length determines fraction of area that burns. Litter must reach a minimum temperature to ignite and enough litter has to have built up. 

#Moisture of extinction: threshold of moisture content in litter/fuel in order for fire to spread (measured as a percent). 15-30% in temperate regions. At the time of the paper, species-specific parameters for flammability were'nt fully incorporated into the model. Thonicke sets woodys to 30%, herbaceous to 20%, but acknowledges that again, it's species specific. I have incorporated the 'litterme' parameter into my PFT's. Decisions about values are discussed later in this script. 

#Litter (fuel) content threshold: above 200 g/m2 and ignition can occur.

###1. Relationship between predicted (m) and observed/measured (mo) fuel moisture content


mo <- seq(0, 1, by=0.01) #vector or possible observed fule moisture content
mp <- 0.4994*mo+1.02 #vector of predicted fuel moisture content
plot(mo,mp)


###2. Probability of at least one fire occuring in a day in a gird cell


#me <- 0.3 #moisture of extinction.  

#e <- exp(1)

#p <- e^(-(pi)(mo/me)^2)
#p <- e^(-(pi)(mp/me)^2) 
#so when soil moisture (mo or me) exceeds moisture extinction (me), fire will not ignite
#plot(p, mp)


###3. Annual length of fire season, N


N <- 30 #sum days with probability of at least 1 fire in a day (p) over whole year. lets just say in this example, it is 30.  


###4. Spread and size of fire


#annual sum of days with particular fire condition, N

N <- 1:365 #vector of possible lengths of fire season, in days. so this is the sum of days that have probability of a least on fire occuring in that day
s <- N/356 #fractional length of fire season




###5. Annual fraction of area burned (fraction of grid cell), A. 

#So A is fucntion of length of the fire season. 


e <- exp(1)

A <- s*e^((s-1)/(0.45*(s-1)^3 + 2.85*(s-1)^2 + 2.96*(s-1) + 1.04)) #fractional area burnt

plot(N,A) 

#---------------------------------------------------------------------------------

##------------Moisture of Extinction------------

#'litterme' -paramater that can be included in a PFT. I used Dimitrakopoulos, A. P., 2001 to come up with some understanding of and values for moisture of exticniton. 


#y=alpha + beta * x
#y = ignition time
#x = moisture content (%)

#rank (less, moderately, flammable, extremely) species by similar flammablity (meditreranean species)
x <- c(0:100, by=1)

#For example: 
#less flammable species: juniperus oxycedrus (juniper) 
b <- 0.532
a <- 16.698

yJuniper <- a+b*x

#flammable speceis: quercus pubescens (valonia oak)
b <- 0.277
a <- 11.512

yQuercus <- a+b*x


plot(x,yJuniper, type="l")
lines(x,yQuercus, type="l")


#References

#Hickler, T., Smith, B., Sykes, M. T., Davis, M. B., Sugita, S., & Walker, K. (2004). Using a generalized vegetation model to simulate vegetation dynamics in northeastern USA. Ecology. 

#Thonicke, K., Venevsky, S., Sitch, S., & Cramer, W. (2001). The role of fire disturbance for global vegetation dynamics: Coupling fire into a dynamic global vegetation model. Global Ecology and Biogeography. 

#Dimitrakopoulos, A. P., & Papaioannou, K. K. (2001). Flammability assessment of Mediterranean forest fuels. Fire Technology. https://doi.org/10.1023/A:1011641601076