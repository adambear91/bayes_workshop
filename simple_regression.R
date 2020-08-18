set.seed(314)
library(tidyverse)
theme_set(theme_bw())


#troll



n <- 20 # number of data points
xmin <- -3
xmax <- 3
b0 <- 103.7
b1 <- -.3
epsilon <- 3

data <- tibble(
   x = runif(n, xmin, xmax),
   y = b0 + b1 * x + rnorm(n, sd = epsilon)
)

reg_scatter <- ggplot(data, aes(x, y)) +
   geom_point(size = .5, shape = 2) +
   scale_x_continuous(
      "Need for Cognition",
      limits = c(xmin, xmax), breaks = c(xmin, 0, xmax)
   ) +
   scale_y_continuous("Shower Temperature", limits = c(95, 115))


## Likelihood ##
log_likelihood <- function(data, b0, b1, epsilon) {
   sum(dnorm(data$y, b0 + b1 * data$x, epsilon, log = TRUE))
}

# Note: We assume epsilon is known.
params <- crossing(b0 = seq(90, 120, .1), b1 = seq(-2, 2, .1))
results <- params %>%
   mutate(
      log_likelihood = pmap_dbl(params, ~log_likelihood(data, .x, .y, epsilon = epsilon))
   )

# Plot likelihood surface
reg_lik_surface <- ggplot(results, aes(b0, b1)) +
   geom_raster(aes(fill = log_likelihood)) +
   geom_contour(aes(z = log_likelihood), color = "white") +
   geom_point(data = filter(results, log_likelihood == max(log_likelihood)), shape = 3) +
   geom_vline(xintercept = b0, linetype = "dashed", alpha = .3) +
   geom_hline(yintercept = b1, linetype = "dashed", alpha = .3) +
   scale_fill_gradient(low = "blue", high = "red") +
   scale_x_continuous("Intercept", breaks = seq(90, 120, 5)) +
   scale_y_continuous("Slope", breaks = -2:2) +
   theme_classic() +
   theme(legend.position = "none")

## Prior ##
mean_b0 <- 105 # known average shower temperature
sd_b0 <- 5
mean_b1 <- 0
sd_b1 <- .5

## Posterior ##
results <- results %>%
   mutate(
      log_posterior = log_likelihood +
         dnorm(b0, mean_b0, sd_b0, log = TRUE) + # prior on b0
         dnorm(b1, mean_b1, sd_b1, log = TRUE)   # prior on b1
   )

# Plot posterior surface
reg_posterior_surface <- ggplot(results, aes(b0, b1)) +
   geom_raster(aes(fill = log_posterior)) +
   geom_contour(aes(z = log_posterior), color = "white") +
   geom_point(data = filter(results, log_posterior == max(log_posterior)), shape = 3) +
   geom_vline(xintercept = b0, linetype = "dashed", alpha = .3) +
   geom_hline(yintercept = b1, linetype = "dashed", alpha = .3) +
   scale_fill_gradient(low = "blue", high = "red") +
   scale_x_continuous("Intercept", breaks = seq(90, 120, 5)) +
   scale_y_continuous("Slope", breaks = -2:2) +
   theme_classic() +
   theme(legend.position = "none")


# Replot data with MLE and MAP fits
reg_fit <- ggplot(data, aes(x, y)) +
   geom_point(size = .5, shape = 2) +
   geom_abline(
      aes(intercept = b0, slope = b1, color = est_type), alpha = .5,
      data = results %>%
         mutate(
            est_type = case_when(
               log_likelihood == max(log_likelihood) ~ "MLE",
               log_posterior == max(log_posterior) ~ "MAP",
               TRUE ~ "NA"
            ) %>%
               fct_relevel("MLE")
         ) %>%
         filter(est_type != "NA")
   ) +
   scale_x_continuous(
      "Need for Cognition",
      limits = c(xmin, xmax), breaks = c(xmin, 0, xmax)
   ) +
   scale_y_continuous("Shower Temperature", limits = c(95, 115)) +
   scale_color_discrete(element_blank()) +
   theme(legend.position = "bottom", legend.box.margin = margin(-10, -10, -10, -10))
