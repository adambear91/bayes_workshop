
library(tidyverse)
library(brms)
library(cowplot)
library(tidybayes)
theme_set(theme_bw())

set.seed(42)

# Load in the data ####

df <- read_rds("trump_data.rds")
head(df)

# This data comes from an experiment in which US subjects gave their opinion on 10 policy questions (the paper is here https://doi.org/10.1017/S0003055418000795 )
# Before giving their opinion on any questions they were randomly assigned to a treatment or control condition
# In the treatment group the subjects were told the policy position of Donald Trump when asked for their own opinion
# In the control group Trump's position was not provided
# Trump's position on all of these policies was in the conservative direction
# The data are in long format: each row is a response to the policy question and there are 10 rows per subject
# There are 5 variables:
# caseid = indexes subject
# Question = indexes policy question
# Support = whether the subject gave a liberal response (1) or conservative response (0)
# republican = whether the subject identified with the Republican Party (1) or not (0)
# contrump = whether the subject was in the treatment group (1) or control group (0)

unique(df$Question) # these are the 10 policy question issues

# Background checks model ####
# We will fit a model that asks:
# Are Republican subjects more likely to respond conservatively on the question of gun background checks when they learn Trump's position?
# In other words do they follow his cue, and to what extent?

# Get the data
df_gb <- 
  df %>%
  filter(Question == "Guns_Background", republican == 1)

# Fit model
fit_gb <- brm(data = df_gb,
              family = gaussian, 
              # note that we follow the authors of the paper by using the Gaussian family with a binary outcome, so this is a "linear probability model"
              # there is an active debate about the advantages and disadvantages of LPMs vs. logistic models for binary data
              # e.g. https://psyarxiv.com/4gmbv/ and https://www.alexpghayes.com/blog/consistency-and-the-linear-probability-model/
              formula = Support ~ 1 + contrump,
              prior = c(prior(normal(0.5, 0.5), class = Intercept),
                        prior(normal(0, 0.5),   class = b),
                        prior(exponential(1),   class = sigma)),
              iter = 3000, warmup = 1000, chains = 4, cores = 4, seed = 42,
              file = "fit_gb")

# > Diagnostics ####
summary(fit_gb) # get model summary
plot(fit_gb) # traceplot
pp_check(fit_gb) # posterior predictive check

# > Get the posterior samples from the model ####
samples_gb <- posterior_samples(fit_gb)
head(samples_gb)

# What is the probability that the average treatment effect is smaller than zero?
mean(samples_gb$b_contrump < 0)

# Plot the posterior distribution of the average treatment effect
samples_gb %>%
  ggplot(aes(x = b_contrump)) +
  geom_histogram() +
  stat_pointintervalh(aes(y = 0), point_interval = mean_hdi, .width = 0.95, color = "red")

# We can also compute the treatment group mean by summing the posterior distributions of the intercept and slope
# We can then plot side by side the posterior distribution of the mean in the treatment and control groups
samples_gb %>%
  mutate(treat_mean = b_Intercept + b_contrump) %>%
  pivot_longer(cols = c("b_Intercept", "treat_mean"), # this function puts the samples in long format to help with plotting
               names_to = "group",
               values_to = "value") %>%
  mutate(group = case_when(group == "b_Intercept" ~ "control_mean", TRUE ~ group)) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram() +
  stat_pointintervalh(aes(y = 0), point_interval = mean_hdi, .width = 0.95)

# Why is there more uncertainty about the treatment group mean than the control group mean? There are two reasons here!

# Extending to the multilevel case ####
# Now we are interested in estimating the average treatment effect of the Trump cue across all 10 policy questions, which we can do with a multilevel model

# Get the data
df_multilevel <- 
  df %>%
  filter(republican == 1)

# Fit model
fit_multilevel <- brm(data = df_multilevel,
                      family = gaussian,
                      formula = Support ~ 1 + contrump + (1 | caseid) + (1 + contrump | Question), # note the syntax is similar to lme4
                      prior = c(prior(normal(0.5, 0.5), class = Intercept),
                                prior(normal(0, 0.5),   class = b),
                                prior(exponential(1),   class = sigma),
                                # the above priors are the same as the previous model, but the below priors are specific to the multilevel case
                                prior(exponential(5),   class = sd), # this prior is on the SD of the intercept and slope among the clusters of caseid and Question
                                prior(lkj(2),           class = cor)), # this prior is on the correlation between the intercept and slope in the cluster of Question
                      iter = 3000, warmup = 1000, chains = 4, cores = 4, seed = 42,
                      file = "fit_multilevel")

# Diagnostics and summary
summary(fit_multilevel)
plot(fit_multilevel)

# Extract posterior samples
samples_multilevel <- 
  fit_multilevel %>% 
  spread_draws(b_contrump, # get the fixed effect samples (these are samples of the overall treatment effect)
               r_Question[Question, parameter]) %>% # get the corresponding random effects samples (these are samples of the effects specific to each question)
  ungroup()

head(samples_multilevel)

# Wrangle and summarise the samples for plotting
samples_ml_summary <-
  samples_multilevel %>%
  filter(parameter == "contrump") %>%
  mutate(r_Question_scale = b_contrump + r_Question) %>% # we must add the question random effects to the fixed effect to get the former on the proper scale
  group_by(Question) %>% 
  mean_hdi(r_Question_scale, .width = 0.95)

# Plot
samples_ml_summary %>%
  ggplot(aes(x = reorder(Question, r_Question_scale), y = r_Question_scale)) +
  geom_point(size = 2.5) +
  coord_flip() +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(samples_multilevel$b_contrump)) + # plots line for average effect
  labs(x = "", y = "ATE")

# Multilevel shrinkage: in the multilevel model the estimate of the average treatment effect for the background checks question is closer to zero
# This is because our estimates on the other questions help to inform our estimate on the background checks question (and the former are closer to zero)
# On average this shrinkage improves our out of sample predictive accuracy
samples_gb %>%
  mean_hdi(b_contrump, .width = 0.95) %>%
  bind_rows(samples_ml_summary %>%
              filter(Question == "Guns_Background") %>%
              rename(b_contrump = r_Question_scale) %>%
              select(-Question)) %>%
  mutate(label = c("Not multilevel", "Multilevel")) %>%
  ggplot(aes(x = label, y = b_contrump)) +
  geom_point(size = 2.5) +
  coord_flip() +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "", y = "ATE", title = "ATE of Trump cue on Guns_Background")
