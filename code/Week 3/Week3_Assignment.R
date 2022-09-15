# (1) Approximately how many hours ahead of Sunbury was the peak flow in Lewisburg during the 2011 flood? (2 pt)
  
# 7 Hours. 

# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable? (4 pts)

Knowing the information on time could tell you how fast the water was moving.

# Package scavenger hunt! (12 pts each)

## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.
install.packages("learnPopGen")
library(learnPopGen)

    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
genetic.drift(p0=0.5, Ne=20, nrep=10, time=100, show="p", pause=0.1)
object<-genetic.drift(p0=0.7,show="heterozygosity")
plot(object,show="genotypes")


        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, manipulate a parameter within the function to create a new result.
genetic.drift(p0=0.8, Ne=20, nrep=10, time=100, show="p", pause=0.1)
object<-genetic.drift(p0=0.7,show="heterozygosity")
plot(object,show="genotypes")
        # Common options might be allele frequency, population size, fitness level, etc. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
       
          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.



## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.
install.packages("adiv")
library(adiv)
    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity. 
data(batcomm)
ab <- batcomm$ab
speciesdiv(comm = ab, method = "Simpson", tol = 1e-8)

      # Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
Simpson
F 8.449169
P 6.642843
O 5.191138
C 4.651464

    # After running the function example, modify your script to generate another diversity metric that is NOT part of the example.
speciesdiv(comm = ab, method = "Margalef", tol = 1e-8)
        # If there are two diversity metrics in the example script, neither of these will count as the modified script.
        # Hint: If the function can "only" caluclate Simpson's diversity, the inverse of Simpson's diversity is another common metric. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.

Margalef
F 4.265215
P 3.053598
O 2.906669
C 2.520027
        
          # Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
          # Their calculation can be very tedious by hand - and very fast with a package designed for the operation.



