
# Load libraries and set plot theme
library(tidyverse)
library(brms)
library(tidybayes)
library(cowplot)
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

# Plot the raw data
plot(data$x, data$y)

# Standardise the predictor variable
data$x_std <- as.numeric(scale(data$x, scale = T, center = T))

# Fit OLS regression
fit_ols <- lm(y ~ x_std, data = data)
summary(fit_ols)

# Fit Bayesian regression using brms
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
                 file = "fit_temp") # this saves the model fit to your working directory with the quoted name

# Diagnostics
summary(fit_bayes) # get model summary
plot(fit_bayes) # traceplot
pp_check(fit_bayes) # posterior predictive check

# Get the posterior samples from the model
samples <- posterior_samples(fit_bayes)
head(samples) # inspect
mean(samples$b_Intercept) # this should match the estimate of the intercept in the above summary call
mean(samples$b_x_std) # this should match the estimate of x

samples %>% # we can also visualise the full posterior distribution for a given parameter
  ggplot(aes(x = b_x_std)) + # eg this is the slope parameter
  geom_histogram() +
  geom_vline(xintercept = mean(samples$b_x_std), color = "red")

samples %>% # and also the joint posterior distribution of multiple parameters
  ggplot(aes(x = b_x_std, y = b_Intercept)) + # eg this is the intercept and slope parameter - what does the plot tell us?
  geom_point(alpha = .3) +
  geom_vline(xintercept = mean(samples$b_x_std), color = "red") +
  geom_hline(yintercept = mean(samples$b_Intercept), color = "red")

# We can also query the posterior distribution with particular questions such as ...
mean(samples$b_x_std < 0) # what is the probability that the value of the slope parameter is smaller than zero? (Conditional on the model and data)
mean(samples$b_x_std > 1) # what about greater than one?

# Plotting posterior inference against the data
# Using lines to indicate uncertainty
data %>%
  ggplot(aes(x = x_std, y = y)) +
  geom_abline(intercept = mean(samples$b_Intercept), # plots the average line from the posterior distribution
              slope     = mean(samples$b_x_std), color = "red", size = 1.5) +
  geom_abline(intercept = samples[1:50, 1], # plots many lines from the posterior distribution
              slope     = samples[1:50, 2], color = "black", alpha = 0.3) +
  geom_point() +
  labs(x = "Need for cognition (std)", y = "Shower temperature preference") +
  #geom_smooth(method = "lm", alpha = 0) +
  NULL

# Using shaded interval to indicate uncertainty
# To do this we need to discretize the predictor and compute the posterior distribution of the mean for each discrete predictor value
# Here is how you would do this for a single discrete value of the predictor (x = 1)
mu_at_1 <- samples %>%
  mutate(mu_at_1 = b_Intercept + b_x_std * 1) # remember the mean in our model is the sum of the intercept and slope

# Plot
(p_mu_at_1 <- mu_at_1 %>%
  ggplot(aes(x = mu_at_1)) +
  geom_histogram() +
  stat_pointintervalh(aes(y = 0), point_interval = mean_hdi, .width = 0.95) +
  xlab(expression(mu["temp | NFC = 1"]))
)

# To generalise this to all discrete values of the predictor (that we care about) we can use the fitted function
x_values <- seq(-2, 2, length.out = 5) # specify discrete values
newdata <- tibble(x_std = x_values) # put in dataframe

mu_long <- fitted(fit_bayes, newdata = newdata, summary = F) %>% # the summary as false returns the full posterior distribution
  as_tibble() %>%
  set_names(x_values) # sets the column names as our discrete values of the predictor variable

head(mu_long) # inspect
mean(mu_long$`1`) # this should match the mean value of mu_at_1
mean(mu_at_1$mu_at_1)

mu_summary <- fitted(fit_bayes, newdata = newdata, summary = T) %>% # the summary as true returns a summary of the full posterior distribution
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

# So far we have computed the mean (and its uncertainty) for discrete values of the predictor
# Now suppose we want to compute plausible observed values of y (rather than the mean); this is called the posterior predictive distribution
# How do we do this?
# We need to additionally take into account the posterior distribution over sigma
# Here is how you would do this for a single discrete value of the predictor (x = 1)
y_at_1 <- samples %>%
  mutate(y_at_1 = rnorm(n = nrow(.), mean = b_Intercept + b_x_std * 1, sd = sigma)) # now we are sampling data from the model, averaged over the posterior

# Plot
(p_y_at_1 <- y_at_1 %>%
  ggplot(aes(x = y_at_1)) +
  geom_histogram() +
  stat_pointintervalh(aes(y = 0), point_interval = mean_hdi, .width = 0.95) +
  xlab(expression(y["temp | NFC = 1"]))
)

# We can plot the posterior predictive distribution at x = 1 next to the posterior distribution of the mean (mu) at x = 1
# Notice that the spread of expected y values is larger than the spread of expected mean values
# This makes sense because we have incorporated sigma into our predictions
plot_grid(p_mu_at_1 + xlim(85, 120) + ylim(0, 2100), p_y_at_1 + xlim(85, 120) + ylim(0, 2100))

# We can generalise this strategy to all discrete values of the predictor (that we care about) using the predict function
y_long <- predict(fit_bayes, newdata = newdata, summary = F) %>% # the summary as false returns the full posterior predictive distribution
  as_tibble() %>%
  set_names(x_values)

head(y_long) # inspect
mean(y_long$`1`) # this should approximately match the mean value of y_at_1 (approximately because of sampling variation)
mean(y_at_1$y_at_1)

# As before we can use predict to conveniently summarise the posterior predictive distribution, and then overlay the summary on our previous plot
y_summary <- predict(fit_bayes, newdata = newdata, summary = T) %>% # notice summary now equals true
  as_tibble() %>%
  bind_cols(newdata)

head(y_summary)

data %>% # now we can plot uncertainty in the model's posterior prediction interval on top of our previous plot
  ggplot(aes(x = x_std)) +
  geom_point(aes(y = y)) +
  geom_smooth(data = mu_summary,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "red", color = "red", alpha = 0.2, size = 1.5) +
  geom_ribbon(data = y_summary,
              aes(ymin = Q2.5, ymax = Q97.5),
              fill = "red", alpha = 0.2) +
  labs(x = "Need for cognition (std)", y = "Shower temperature preference") +
  #geom_smooth(method = "lm", alpha = 0.2) +
  NULL

# Mean-centering in brms ####
# Remember how we standardized the predictor variable in our previous model? This is helpful for several reasons
# 1. It makes it easier to set priors on the intercept
# 2. It makes MCMC sampling more efficient
# 3. brms internally centers your predictor variables before sampling (because of 2.)
# Read more here https://bookdown.org/content/3890/horoscopes-insights.html#use-the-0-intercept-syntax

# Let's fit a model where we don't standardize/center the predictor

# Plot unstandardized x against y
plot(data$x, data$y)

# Fit Bayesian regression using brms
fit_bayes_2 <- brm(data = data,
                   family = gaussian, # specify model family
                   formula = y ~ 1 + x, # model formula, with x unstandardized/uncentered
                   prior = c(prior(normal(105, 5),    class = Intercept), # this is the prior on the intercept
                             prior(normal(0, 1),      class = b), # on the slope
                             prior(exponential(0.33), class = sigma)), # on the noise term
                   iter = 2000, # number of samples from the posterior distribution per chain
                   warmup = 1000, # number of samples per chain that are used for optimising the sampling procedure
                   chains = 4, # number of chains
                   cores = 4, # number of cores to use, limited by the hardware you are using
                   seed = 42,
                   file = "fit_temp_2") # this saves the model fit to your working directory with the quoted name

summary(fit_bayes_2)
samples_2 <- posterior_samples(fit_bayes_2) # get posterior samples

# Plot the joint posterior distribution of the intercept and slope parameters - now they are positively correlated, why?
samples_2 %>%
  ggplot(aes(x = b_x, y = b_Intercept)) +
  geom_point(alpha = .3) +
  geom_vline(xintercept = mean(samples_2$b_x), color = "red") +
  geom_hline(yintercept = mean(samples_2$b_Intercept), color = "red")

# Hint:
plot(data$x, data$y)
