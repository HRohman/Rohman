setwd("C:/GitHub/Rohman/code/Final Project")
install.packages("mgcv")
library(mgcv)
install.packages("MuMIn")
library(MuMIn)

#Transferring data from excel into r
lizards <- read.csv("MR.csv")
zhaoqing <- read.csv("Zhaoqing.csv")
chuzhou <- read.csv("Chuzhou.csv")

#Cleaning up the data sets to only data I need
lizards2 <- lizards[-1:-32,c(-1,-3:-5,-7:-17)]
zha.lizards <- lizards2[-23:-50,]
chu.lizards <- lizards2[-1:-22,]
chuzhou2 <- chuzhou[,-4]
zhaoqing2 <- zhaoqing[,-4]
chuzhou2$TempDiff <- chuzhou2$Day-chuzhou2$Night
zhaoqing2$TempDiff <- zhaoqing2$Day-zhaoqing2$Night
zhaoqing2$Species <- c("T.sexlineatus","T.sexlineatus","T.sexlineatus","T.sexlineatus","T.sexlineatus","T.sexlineatus","T.sexlineatus","T.sexlineatus","T.sexlineatus","T.sexlineatus","T.sexlineatus","T.sexlineatus")
chuzhou2$Species <- c("T.wolteri","T.wolteri","T.wolteri","T.wolteri","T.wolteri","T.wolteri","T.wolteri","T.wolteri","T.wolteri","T.wolteri","T.wolteri","T.wolteri")

#Averaging out the data
zhaliz.mean <- aggregate(zha.lizards$RMR18C.ml.g.hr. ~ zha.lizards$Species, FUN = "mean")
head(zhaliz.mean)
chuliz.mean <- aggregate(chu.lizards$RMR18C.ml.g.hr. ~ chu.lizards$Species, FUN = "mean")
head(chuliz.mean)
chu.mean <- aggregate(chuzhou2$TempDiff ~ chuzhou2$Species, FUN = "mean")
head(chu.mean)
zha.mean <- aggregate(zhaoqing2$TempDiff ~ zhaoqing2$Species, FUN = "mean")
head(zha.mean)

#Renaming for merging purposes
colnames(zha.mean) <- c("Species","TempDiff")
colnames(chu.mean)<- c("Species","TempDiff")
colnames(zhaliz.mean)<- c("Species","MetabolicRate")
colnames(chuliz.mean) <- c("Species","MetabolicRate")


#Merging
lizards2.0 <- rbind(chuliz.mean,zhaliz.mean)
temps <- rbind(chu.mean,zha.mean)
Alldata <- merge(lizards2.0,temps, by = "Species")

#Analyzing 
mod1 <- gam(MetabolicRate ~ TempDiff,family = Gamma, random = list(ID=~1),data = Alldata)
mod2 <- gam(MetabolicRate ~ Species,family = Gamma, random = list(ID=~1),data = Alldata)
AIC(mod1,mod2)

#graphs
plot(Alldata$MetabolicRate,Alldata$TempDiff,xlim = c(0.150,0.240),xlab = "Metabolic Rate of Lizards", ylab = "Temperature Difference",col = "snow", main = "Temperature Fluctuation (Day - Night) Compared with Metabolic rates of Lizards")
text(Alldata$MetabolicRate,Alldata$TempDiff, labels = Alldata$Species)

barplot(lizards2.0$MetabolicRate~lizards2.0$Species, xlab = "Species",ylab = "Metabolic Rate", main = "Metabolic Rates for two Lizard species in China")
boxplot(Alldata$TempDiff*Alldata$MetabolicRate ~ Alldata$Species,xlab = "Species",ylab = "Metabolic Rate times ∆Temperature", main = "Metabolic Rates and Temperature Difference comapred by species" )

