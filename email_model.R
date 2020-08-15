library(tidyverse)
theme_set(theme_bw())

#### Simple Model ####
## Assumptions ##
# Conditional on an eventual response, expected future wait time increases
#  the longer you've been waiting (corresponding to Weibull dist. with k < 1)

## Unknowns ##
# Will the professor reply to my email? [Logical]

## Data ##
# The amount of time that's passed without receiving email [Positive Scalar]

## Likelihood ##
# p(x days passed | eventual response) = 1 - Weibull CDF
# p(x days passed | no eventual response) = 1

results <- tibble(days_passed = 0:400) %>% # data for time without a response
   mutate(
      # scale and shape for Weibull give median ~10 days wait time
      likelihood_response = 1 - pweibull(days_passed, shape = .5, scale = 21),
      likelihood_noresponse = 1
   )

# Plot likelihoods
results %>%
   pivot_longer(
      -days_passed,
      names_to = "outcome",
      names_prefix = "likelihood_",
      values_to = "likelihood"
   ) %>%
   ggplot(aes(days_passed, likelihood, color = outcome)) +
   geom_line(size = 1) +
   scale_x_continuous("Days without Response (Data)") +
   scale_y_continuous("Probability of Not Receiving Response") +
   scale_color_discrete("Will they ever respond?", labels = c("No", "Yes")) +
   theme(legend.position = "bottom")

## Prior ##
p_response <- .75 # prior that you'll get eventual response

results_simple <- results %>%
   mutate(
      posterior_response = p_response * likelihood_response /
         (p_response * likelihood_response +
         (1 - p_response) * likelihood_noresponse),
      posterior_noresponse = (1 - p_response) * likelihood_noresponse /
         (p_response * likelihood_response +
         (1 - p_response) * likelihood_noresponse)
   )

# Plot posterior as a function of days passed
results_simple %>%
   select(-starts_with("likelihood")) %>%
   pivot_longer(
      starts_with("posterior"),
      names_to = "outcome",
      names_prefix = "posterior_"
   ) %>%
   ggplot(aes(days_passed, value, color = outcome)) +
   geom_line(size = 1) +
   scale_x_continuous("Days without Response (Data)") +
   scale_y_continuous("Posterior Belief in Eventual Outcome") +
   scale_color_discrete("Will they ever respond?", labels = c("No", "Yes")) +
   labs(caption = str_c("Prior belief in eventual response: ", 100*p_response, "%")) +
   theme(legend.position = "bottom")


#### Alternative Model ####
p_forgot <- .15 # prior that receiver will forget to respond
p_ignore <- .25 # prior that receiver will intentionally ignore email

results_alt <- results %>%
   mutate(
      posterior_response = (1 - p_forgot - p_ignore) * likelihood_response,
      posterior_forgot = p_forgot * likelihood_noresponse,
      posterior_ignore = p_ignore * likelihood_noresponse
   ) %>%
   rowwise() %>%
   mutate(normalization = sum(c_across(starts_with("posterior")))) %>%
   ungroup() %>%
   pivot_longer(
      starts_with("posterior"),
      names_to = "outcome",
      names_prefix = "posterior_",
      values_to = "posterior"
   ) %>%
   mutate(posterior = posterior / normalization)

ggplot(results_alt, aes(days_passed, posterior, color = outcome)) +
   geom_line(size = 1) +
   scale_x_continuous("Days without Response (Data)") +
   scale_y_continuous("Posterior Belief", limits = c(0, 1)) +
   scale_color_discrete(
      element_blank(),
      labels = c("Forgot", "Ignoring", "Will respond")
   ) +
   theme(legend.position = "bottom")
