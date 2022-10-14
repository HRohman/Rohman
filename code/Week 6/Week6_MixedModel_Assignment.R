# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."

setwd("C:/Github/Rohman/code/Week 6")
df <- read.csv("Toscano_Griffen_Data.csv")
head(df)
# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from the tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects.
# In the other model include one interactive effect.
glmm.mod <- glmmPQL(activity.level~temperature + toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod)

glmm.mod2 <- glmmPQL(activity.level~claw.width, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod2)
    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.
#Need three predictor variables in each model: y~x1+x2+x3 or y~x1*x2+x3

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
df$prop.cons <- df$eaten/df$prey 
glmm.mod3 <- glmmPQL(prop.cons~claw.width, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod3)
glmm.mod4 <- glmmPQL(prop.cons~temperature + toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod4)



# (Q1) - The code in line 8 is performing two operations at once. What are they? (2 pts)
##The code estimates how much of a random effect individuals have on the data, and it evaluates the distribution as binomial.
# The line creates a proportion and then generates a new column in the data frame.

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)

##Yes. When running the interactive effect with prop.con, temperature becomes a predictor variable since it has a significant p-value. 
#What makes this interactive?

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
plot(glmm.mod3$residuals)
plot(glmm.mod4$residuals)

#THe second model is better. The data follows a tighter pattern, but not by much - neither of the models are a great fit. 
#Both are bad - there is a clear/strong pattern when the goal is random.


# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)
gam.mod1 <- gam(prop.cons~claw.width*carapace.width, family = binomial, random = ~ 1 | block, data = df)
plot(gam.mod1)
gam.mod2 <- gam(prop.cons~temperature + toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
plot(gam.mod2)


# (Q4) - Which model is a better fit? (2 pt)

AIC(gam.mod1, gam.mod2)

##gam.mod1 is a better fit. 


# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)

plot(gam.mod1$residuals)
plot(gam.mod2$residuals)

## I'm not confident in either because the data is so spread out on both models and doesn't follow a trend.
#That is the goal! I wouldn't be very confident in either because there are still some clear trends buried in there.






