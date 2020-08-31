# Load libraries and set plot theme
library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_bw())

# Generate fake data
n <- 20 # number of data points
xmin <- -3
xmax <- 3
b0 <- 103.7
b1 <- -.3
epsilon <- 3

set.seed(314)
data <- tibble(
  x = runif(n, xmin, xmax),
  y = b0 + b1 * x + rnorm(n, sd = epsilon)
)

# Plot the raw data ####
plot(data$x, data$y)

# Standardise the predictor variable ####
data$x_std <- as.numeric(scale(data$x, scale = T, center = T))

# For comparison purposes, fit OLS regression
fit_ols <- lm(y ~ x_std, data = data)
summary(fit_ols)

# Fit Bayesian regression using brms ####
fit_bayes <- brm(data = data,
                 family = gaussian, # specify model family
                 formula = y ~ 1 + x_std, # model formula
                 prior = c(prior(normal(105, 5),    class = Intercept), # this is the prior on the intercept
                           prior(normal(0, 1),      class = b), # on the slope
                           prior(exponential(0.33), class = sigma)), # on the noise term
                 iter = 2000, # number of samples from the posterior distribution per chain
                 warmup = 1000, # number of samples per chain that are used for optimising the sampling procedure
                 chains = 4, # number of chains
                 cores = 4, # number of cores to use, limited by the hardware you are using
                 seed = 42,
                 file = "models/fit_temp") # this saves the model fit to your working directory with the quoted name

# Visualising the priors ####
curve(from = 70, to = 140, dnorm(x, 105, 5))
curve(from = -4, to = 4, dnorm(x, 0, 1))
curve(from = 0, to = 10, dexp(x, 0.33))

# Diagnostics ####
summary(fit_bayes) # get model summary
plot(fit_bayes) # traceplot
pp_check(fit_bayes) # posterior predictive check

# Other error messages you might get
# Divergent transitions! https://mc-stan.org/docs/2_19/reference-manual/divergent-transitions

# Get the posterior samples from the model ####
samples <- posterior_samples(fit_bayes)
head(samples) # inspect
mean(samples$b_Intercept) # this should match the estimate of the intercept in the above summary call
mean(samples$b_x_std) # this should match the estimate of x

# The values in samples are draws from the posterior distribution - we can visualise them for any given parameter
samples %>%
  ggplot(aes(x = b_x_std)) + # eg this is the slope parameter
  geom_histogram() +
  geom_vline(xintercept = mean(samples$b_x_std), color = "red") # with the mean parameter value added as a vertical line

# We can also visualise the joint posterior distribution of multiple parameters
samples %>%
  ggplot(aes(x = b_x_std, y = b_Intercept)) + # eg this is the intercept and slope parameter - what does the plot tell us?
  geom_point(alpha = .3) +
  geom_vline(xintercept = mean(samples$b_x_std), color = "red") +
  geom_hline(yintercept = mean(samples$b_Intercept), color = "red")

# We can also query the posterior distribution with particular questions such as ...
mean(samples$b_x_std < 0) # what is the probability that the value of the slope parameter is smaller than zero? (Conditional on the model and data)
mean(samples$b_x_std > 1) # what about greater than one?

# Plotting posterior inference against the data ####
# There are a few ways we can do this with our simple linear regression

# Using lines to indicate uncertainty
data %>%
  ggplot(aes(x = x_std, y = y)) +
  geom_abline(intercept = mean(samples$b_Intercept), # plots the average line from the posterior distribution
              slope     = mean(samples$b_x_std),
              color = "red", size = 1.5) +
  geom_abline(intercept = samples[1:50, 1], # plots many lines from the posterior distribution
              slope     = samples[1:50, 2],
              color = "black", alpha = 0.3) +
  geom_point() +
  labs(x = "Need for cognition (std)", y = "Shower temperature preference") +
  #geom_smooth(method = "lm", alpha = 0) + # plots the OLS regression line
  NULL

# Using shaded interval to indicate uncertainty
# To do this we need to discretize the predictor and compute the posterior distribution of the mean for each discrete predictor value
# Here is how you would do this for a single discrete value of the predictor (x = 1)

# Compute the mean when x = 1
mu_at_1 <-
   samples %>%
   mutate(mu_at_1 = b_Intercept + b_x_std * 1) # remember the mean in our model is the sum of the intercept and slope

head(mu_at_1)

# Plot
mu_at_1 %>%
   ggplot(aes(x = mu_at_1)) +
   geom_histogram() +
   stat_pointintervalh(aes(y = 0),
                       point_interval = mean_hdi,
                       .width = 0.95) +
   xlab(expression(mu["temp | NFC = 1"]))

# To generalise this to all discrete values of the predictor (that we care about) we can use the fitted function

x_values <- seq(-2, 2, length.out = 5) # specify discrete values of the predictor variable
newdata <- tibble(x_std = x_values) # put in dataframe

# Get the full posterior distribution of the mean for our values of the predictor
mu_long <-
   fitted(fit_bayes,
          newdata = newdata,
          summary = F) %>% # note the summary = false
   as_tibble() %>%
   set_names(x_values) # sets the column names as our discrete values of the predictor variable

head(mu_long) # inspect

mean(mu_long$`1`) # this should match the mean value of mu_at_1
mean(mu_at_1$mu_at_1)

quantile(mu_long$`1`, probs = c(.025, .975)) # this should match the lower and upper limits of mu_at_1
quantile(mu_at_1$mu_at_1, probs = c(.025, .975))

# Get a summary of the posterior distribution of the mean for our values of the predictor
mu_summary <-
   fitted(fit_bayes,
          newdata = newdata,
          summary = T) %>% # note the summary = true
   as_tibble() %>%
   bind_cols(newdata) # adds the values of the predictor variable as a new column

head(mu_summary) # inspect

data %>% # now we can plot the shaded region using the geom smooth function and mu_summary
  ggplot(aes(x = x_std, y = y)) +
  geom_point() +
  geom_smooth(data = mu_summary,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "red", color = "red", alpha = 0.2, size = 1.5) +
  labs(x = "Need for cognition (std)", y = "Shower temperature preference") +
  #geom_smooth(method = "lm", alpha = 0.2) + # this line adds the OLS regression line to the plot
  NULL

