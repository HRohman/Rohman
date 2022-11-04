# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.
#Where is the library for reading the spreadsheets in as data frames?
#Where is your working directory?
setwd("C:/GitHub/Rohman/code/Week 9")
invert.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
Abiotic <- as.data.frame(abiotic.tibble)#typo here - also where is your abiotic data for reading into the script?

invert <- as.data.frame(invert.tibble)
head(invert)
head(abiotic) #Abiotic is upper case above and lower case here.
abiotic.names <- paste(abiotic$parcel)#Type here too.
abiotic$names <- abiotic.names
invert.names <- paste(invert$Parcel, invert$Landuse)
invert$names <- invert.names
abiotic.mean <- aggregate (x = invertebrate, by = list(invertebrate$names), FUN = "mean") #different names between object and function input. Also different intended data (abiotic vs invert) that will cause serious issues downstream. 
abiotic.mean1 <- abiotic.mean[,-16]
head(abiotic.mean)
abotic.mean2 <- abiotic.mean[,-2:-3]
head(abiotic.mean2)
abiotic.mean3 <- abiotic.mean2[,-3:-4]
invert.means <- aggregate(x = invert, by = list(invert$names), FUN = "mean")
head(invert.means)
invert.means2 <- invert.means[,-2,-3] 
head(invert.means2)
invert.means3 <- invert.means[,-71]
head(invertebrate.mean3)

library(vegan)
colnames(abiotic.mean3)
install.packages("plyr")
library(plyr)
ab.invert <-rbind.fill(abiotic.mean3, invert.means3)
ab.invert2 <- medrge(abiotic.mean3, invert.means3, by = "group.1")
ab.invert3 <- ab.invert2[,-1]
ord <- rda(ab.invert3)
ord

## There is a bigger impact on the plant community than the invertebrate community. This is because if there are plants only growing due to those abiotic factors, then only invertebrates that feed on those specific plants would be effected. 
# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.

library(fitdistrplus)
library(logspline)
fit.weibull <- fitdist(ab.invert3pH, distr = "weibull")
fit.gamma <- fitdist(ab.invert3$pH, distr = "gamma")
fit.norm <- fitdist(ab.invert3$pH, distr = "norm")
gofstat(list(fit.weibull,fit.gamma,fit.norm))
colnames(ab.invert3)
mod1 <- lm(pH ~Plot + totalN + Trichia_hispida + Opomyza_sp,ab.invert3)
anova(mod1)
AIC(mod1)
#Lines 34-55 are identical to snippets from someone you sit next to...but nonfunctional in your script.

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.

## RDA shows if there is a relationship between the two factors and whether that relationship is significant. The linear model helps you identify which factor have the biggest impact and how significant that impact is. You have to use both models together to see a complete picture. 

