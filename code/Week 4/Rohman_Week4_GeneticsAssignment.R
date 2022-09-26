# Look at the plot and model results for our Dryad data in the tutorial. Part 1: Without knowing which points represent which groups, 
  # give one explanation for why these data might be difficult to draw spatial inferences about genes.(3 points)

## It might be difficult to draw spatial inferences because there is only one main area where the data is clustered together. 
##The rest of the data is scattered randomly so it would be really difficult to come up with a clear inference about diversity and where it occurs. 


  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (3 points), and EXPLAIN WHY (4 points).

## The colors denote regions where the genotypes are significantly different.So the light blue dots that are close together denote one, unique genotype that occurs 
## at only that specific location, while the black dots are a more generalized genotype that is highly adaptable and can occur in many other parts of the world. 


# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)
install.packages("stability")
library(stability)
data("ge_data")
data <- ge_data

# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.
mod.env <- lm(data$Yield ~ data$Env)
anova(mod.env)
mod.gen <- lm(data$Yield ~ data$Gen)
anova(mod.env)

# Test the significance of both models and look at the model summary. (3 points each)
summary(mod.env)
summary(mod.gen)
  # Which model is a better fit to explain the yield response, and WHY? (6 points)

## The environment model is a better fit because more of the data is significant that in the genotype model. This can be understood by looking at the little stars next to the data. 
## The genotype model has more data but less stars, while the environment model has less data but a greater amount of significance. 

  # Hint: Does one model seem more likely to be over-fitted?

# Which environment would be your very WORST choice for generating a strong yield response? (2 points)

##Sargodha would be the worst choice for generating a strong yield response. The p-value is 0.7138.