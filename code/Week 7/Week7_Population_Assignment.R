# Load the "anytime" and "ggplot2" packages to complete this week's assignment.
install.packages("anytime")
install.packages("ggplot2")
library(anytime)
library(ggplot2)

# Read the "Plankton_move_average" CSV in from GitHub. 
# These are data from the Great Lakes Environmental Research Laboratory plankton sampling.
setwd("C:/Github/Rohman/code/Week7")
read.csv("Plankton_move_average.csv")
data <- read.csv("Plankton_move_average.csv")

#Used the following lines to format the date and remove NAs from the dataset:
data$Date <- as.Date(data$Date, origin = "0001-01-01") # Setting values to "day zero".
data <- na.omit(data)

#Plot these population data over time with the following code:
ggplot(data)  +
  xlab("Numeric Date") + ylab("Density Individuals")+
  geom_line(data=data, aes(Date, D.mendotae), color="black", alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, LimncalanusF+LimncalanusM), color="orange",  alpha = 0.7, size=1)+ # adding males and females together, hint: this is actually spelled Limnocalanus
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  theme_bw() 

# Export this plot to have on hand for reference in the next section of the assignment (and upload with your script).

# (1) - Which species is most likely to be r-selected prey and which its primary predator? (2 pts)
### The black line is the r-selected prey and the orange line is it's primary predator. 
#Which species are those representing?

# What is one relationship the third species MIGHT have to the first two? (2 pts)
### The third species could be another predator that is in competition with the orange line. 

#Now copy/paste in the Lotka-Volterra function, plotting script, and load the "deSolve" package from the tutorial:
library(deSolve)
LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)
## Alpha represents prey reproduction coefficient, beta represents the rate of predation, Gamma represents stability, and delta is the rate of prey consumption.
# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.
Pars <- c(alpha = 2.8, beta = 0.5, gamma = .3, delta = .6)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)
plot <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
matplot(plot[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Limnocalanus", "D.mendotae"), lty = c(1,2), col = c(1,2), box.lwd = 0)

# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)
## I changed the alpha value. It says that the Limnocalanus is more affected by changes in prey reproduction than D.mendotae is. 
#That means you changed the rate of D.mendotae reproduction...I think they might have an opinion about how much it effects them.

# Are there other paramenter changes that could have created the same end result? (2 pts)
## Maybe beta? Decreasing the rate of predation would sort of superficially make the rate of reproduction appear like its increasing since there would be more individuals. 

# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
# and upload with your script. (hint - remember which one is the predator and which is the prey)




