# Generalised Linear Modelling (outcome variables do NOT follow the normal distribution)

# Using the 'cars' dataset from the mtcars package for this
# For a description of this dataset, run ?mtcars
head(mtcars)

# What variables are strongly associated with manual vs automatic transmission? Response variable here is
# either 0 = automatic, or 1 = manual
library(tidyverse)
mtplot_1 <- ggplot(data = mtcars, aes(y = am, x = mpg)) +
  geom_jitter(width =0, height =0.01, alpha =0.5, colour ="#984ea3")
mtplot_1 # Plot tells us that, for this dataset, manual cars tend to have higher miles per gallon)

mtplot_1 + geom_smooth(method = "lm", se = FALSE)
# Line tells us little as it places binary values between 1 and 0. Obviously, it's also not
# possible for binary values to follow a normal distribution...

# A logistic regression model, on the other hand, allows us to produce probabilities of falling into
# these two categories.

mtplot_1 + geom_smooth(method = "glm",
                     method.args = list(family = "binomial"), se = FALSE) # Can see here that the line
# no longer plots values above 1 or below 0. This line echoes what was mentioned before, that cars with
# higher mpg are more likely to fall into the manual category (1).

# Now, we can fit this logistic regression model and show the output
mt_glm <- glm(am ~ mpg, family = "binomial", data = mtcars)
summary(mt_glm)

