library(tidyverse)
theme_set(theme_bw())

## Assumptions ##
# Adam is lactose intolerant and gets disutility from eating food with lactose.
# Chocolate desserts are not more/less likely to have lactose than other desserts.
# Adam eats non-chocolate desserts 50% of the time.

## Unknowns ##
# Does cake contain lactose? [Logical]
# How much does Adam like chocolate desserts relative to non-chocolate? [Positive Scalar]

## Data ##
# Adam eats a chocolate desert.

## Likelihood ##
# p(Adam eats chocolate dessert | has_lactose, b_chocolate) = logistic(utility), where
#   utility = b_chocolate * is_chocolate - has_lactose

likelihood <- function(has_lactose, b_chocolate, temperature) {
   utility <- b_chocolate - has_lactose
   1 / (1 + exp(-utility/temperature))
}

# Calculate likelihoods over sample of parameter space
x_step <- .01 # size of steps to take over b_chocolate
b_vals <- seq(0, 2, x_step) # values of b_chocolate to evaluate
params <- crossing(has_lactose = c(TRUE, FALSE), b_chocolate = b_vals)
results <- params %>%
   mutate(likelihood = pmap_dbl(params, likelihood, temperature = .1))

# Plot likelihoods
cake_lik_plot <- ggplot(results, aes(b_chocolate, likelihood, color = has_lactose, linetype = has_lactose)) +
   geom_line(size = 1) +
   geom_vline(xintercept = 1, linetype = "dashed", alpha = .5) +
   scale_x_continuous("Preference for Chocolate vs. Non-chocolate Desserts") +
   scale_y_continuous("Probability of Eating Chocolate Dessert", limits = c(0, 1)) +
   scale_color_discrete("Has lactose?", labels = c("No", "Yes")) +
   scale_linetype_discrete("Has lactose?", labels = c("No", "Yes")) +
   theme(legend.position = "bottom")

## Prior ##
p_has_lactose <- .8
p_b_chocolate <- dnorm(b_vals, .75, .15) * x_step # convert to PMF for grid approximation

## Bayesian Update ##
results <- results %>%
   mutate(
      prior = has_lactose * p_has_lactose * p_b_chocolate + (1 - has_lactose) * (1 - p_has_lactose) * p_b_chocolate,
      posterior = (prior * likelihood) / sum(prior * likelihood)
   )

# Plot prior and posterior
cake_posterior_plot <- results %>%
   select(-likelihood) %>%
   pivot_longer(c(prior, posterior), names_to = "quantity") %>%
   mutate(quantity = fct_relevel(quantity, "prior")) %>%
   ggplot(aes(b_chocolate, value, color = has_lactose, linetype = has_lactose)) +
   geom_line(size = 1) +
   facet_wrap(vars(quantity), labeller = as_labeller(str_to_title)) +
   scale_x_continuous("Preference for Chocolate vs. Non-chocolate Desserts") +
   scale_y_continuous("Probability", breaks = NULL) +
   scale_color_discrete("Has lactose?", labels = c("No", "Yes")) +
   scale_linetype_discrete("Has lactose?", labels = c("No", "Yes")) +
   labs(caption = str_c(
      "Prior dessert contains lactose: ", 100*p_has_lactose, "%, ",
      "Posterior dessert contains lactose: ", with(results, 100*sum(posterior[has_lactose])) %>% round(), "%"
   )) +
   theme(legend.position = "bottom", plot.caption = element_text(hjust = .5))

