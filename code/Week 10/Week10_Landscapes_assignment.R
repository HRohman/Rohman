# Load the packages from this week's tutorial.
setwd("C:/GitHub/Rohman/code/Week 10")
library(spdep)
library(adespatial)
library(vegan)
#In the tutorial we looked at the community as a whole and the swimmers which ultimately matched a prediction we had for their distribution.

#Part 1: Look at two other subsets of the community and determine the relative influence of space and habitat on each following the methods in the tutorial. (10 points)
#The options include groupings by taxonomy, where Diptera (true flies) have the strongest flight ability, Trichoptera the 2nd strongest, 
    #Ephememeroptera are 3rd, and non insects are 4th...because they don't have wings.
#Groupings by habits include the swimmers (off limits for the assignment) as most mobile, sprawlers as 2nd (they move in search of food, but not quickly),
    #and the clingers come in last (they might move up and down on individual rocks).
PatchLatLon.csv <- read.csv("PatchLatLon.csv", header=T)
BugsByPatch.csv <- read.csv("BugsByPatch.csv", header=T)
HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
Clingers.csv <- read.csv("Clingers.csv")
Diptera.csv <- read.csv("Diptera.csv")
PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
BugsByPatch.mat <- as.matrix(BugsByPatch.csv)
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Cling.mat <- as.matrix(Clingers.csv)
Diptera.mat <- as.matrix(Diptera.csv)

nb<-cell2nb(3,30,"queen") 
nb1 <- droplinks(nb, 19, sym=TRUE) 
nb2 <- droplinks(nb1, 22, sym=TRUE)
nb3 <- droplinks(nb2, 25, sym=TRUE)
nb4 <- droplinks(nb3, 30, sym=TRUE)
bin.mat <- aem.build.binary(nb4, PatchLatLon.mat, unit.angle = "degrees", rot.angle = 90, rm.same.y = TRUE, plot.connexions = TRUE)
plot(PatchLatLon.mat[,2]~PatchLatLon.mat[,3], xlim = rev(c(76.75,77)))
aem.ev <- aem(aem.build.binary=bin.mat)
aem.df <- aem.ev$vectors[c(-19,-22,-25,-30),]
Space.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df))
Space.r2a <- RsquareAdj(Space.rda)$adj.r.squared
aem.fwd <- forward.sel(BugsByPatch.mat,aem.df, adjR2thresh=Space.r2a)
SpaceNoHab.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)
HabNoSpace.rda <- rda(BugsByPatch.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)

ClingSpace.rda <- rda(Cling.mat, as.data.frame(aem.df))
ClingSpace.r2a <- RsquareAdj(ClingSpace.rda)$adj.r.squared
Clingaem.fwd <- forward.sel(Cling.mat,as.data.frame(aem.df), adjR2thresh=Space.r2a)
ClingSpaceNoHab.rda <- rda(Cling.mat, as.data.frame(aem.df[,Clingaem.fwd$order]), HabitatbyPatch.mat)
ClingSpaceNoHab.rda 
anova(ClingSpaceNoHab.rda, perm.max = 10000)
RsquareAdj(ClingSpaceNoHab.rda)
#conditional: 25.8%   constrained: 48.5%     

ClingHabNoSpace.rda <- rda(Cling.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,Clingaem.fwd$order]))
ClingHabNoSpace.rda 
anova(ClingHabNoSpace.rda, perm.max = 10000)
RsquareAdj(ClingHabNoSpace.rda)
#conditional: 70%  constrained: 4.4%


DipteraSpace.rda <- rda(Diptera.mat, as.data.frame(aem.df))
DipteraSpace.r2a <- RsquareAdj(DipteraSpace.rda)$adj.r.squared
Dipteraaem.fwd <- forward.sel(Diptera.mat,as.data.frame(aem.df), adjR2thresh=Space.r2a)
DipteraSpaceNoHab.rda <- rda(Diptera.mat, as.data.frame(aem.df[,Dipteraaem.fwd$order]), HabitatbyPatch.mat)
DipteraSpaceNoHab.rda 
anova(DipteraSpaceNoHab.rda, perm.max = 10000)
RsquareAdj(DipteraSpaceNoHab.rda)
#conditional: 40%    constrained: 42% 

DipteraHabNoSpace.rda <- rda(Diptera.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,Dipteraaem.fwd$order]))
DipteraHabNoSpace.rda 
anova(DipteraHabNoSpace.rda, perm.max = 10000)
RsquareAdj(DipteraHabNoSpace.rda)
#conditional: 77%    constrained: 4.8%

#SpaceNoHab - 26% conditional  46% constrained 
#HabNoSpace - 67% conditional  4.9% constrained 

#CLINGERS
## SpaceNoHab:  conditional: 25.8%  constrained: 48.5%  
## HabNoSpace:  conditional: 70%    constrained: 4.4%

#DIPTERA
## SpaceNoHab:  conditional: 40%    constrained: 42% 
## HabNoSpace:  conditional: 77%    constrained: 4.8%


#Part 2: What is your interpretation of the pattern for each group individually, and the two in comparison, based on their mobility? (5 points)

## The values for the Clingers community largely stayed the same, but there was the largest difference (of 3%) when there was a habitat and no space. This makes sense because due to
## clingers lifestyle, hanging on rocks, that they would be much more affected by changes in habitat instead of changes in how much space is available. The values for Diptera saw
## greater variability. The greatest change was a 14% increase in conditional for space with no habitat. This is because the Diptera are strong fliers and thus would need a large habitat 
## to fly around in! 


#Part 3: For each of your chosen groups of bugs, perform variable selection for the habitat data rather than the AEM data. Which habitat variables are significant for each? (10 points)
  # Definitions for the habitat column names:
    #Inorg = total suspended inorganic solids in the water column
    #Organ = total suspended organic solids in the water column
    #Chla = Chlorophyll a concentration from benthic rocks collected in-stream
    #BOM = total benthic organic matter in the sample
    #Depth = water depth
    #Flow	= water velocity where samples were collected
    #Fines = Percent of the substrate as "fines" i.e. small particles too small to measure
    #AveAr = The average size of rocks where each sample was collected

cling.vs <- rda(Clingers.csv~., HabitatbyPatch.csv)
cling.vs

### The RDA with the smallest value is the AveAr. This makes sense because Clingers are dependent on rocks for all of their processes. 

Diptera.vs <- rda(Diptera.csv~., HabitatbyPatch.csv)
Diptera.vs

### There are three zeroes for AveAr, Fines, and Flow. This makes sense because Diptera are fliers, and therefore would not be super affected by rock size/substrate particles/water velocity.


#Part 4: How do you expect selecting both the spatial and the habitat variables would change the results of the RDAs from Part 1 above? (5 points)
  #(You do not need to redo the RDAs, unless you *want* to.)

### Selecting for spatial variables wouldn't affect the clingers that much since they do not require a lot of space just to hang on to the same rock. Diptera's RDA, however
### would be affected since they need a lot more space to fly around. 