library(bayesplot)
library(haven)  
library(bnlearn)  
library(gRain) 
library(ggplot2)  
library(dplyr) 
library(brms)
library(tidyverse)
library(mice) 
library(mcmcplots)
library(rethinking)
library(patchwork)
library(viridis)
library(ggthemes)

theme_set(theme_tufte())
#library(kableExtra)
data <- read_sav("Untitled26.sav")
data$D104 <- factor(data$D104, levels = c("1", "2", "3", ordered = TRUE))
data$D102 <- factor(data$D102, levels = c("1", "2", "3", "4", ordered = TRUE))

modeldata <- data %>%
  mutate(across(starts_with("M201_"), ~if_else(. == -9, NA_real_, .))) %>%
  mutate(across(starts_with("M209_"), ~if_else(. == -9, NA_real_, .))) %>%
  mutate(across(starts_with("M210_"), ~if_else(. == -9, NA_real_, .))) %>%
  mutate(D104 = D104,
         Expertise = case_when(
           D102 == 1 ~ "Human Factors",
           D102 == 2 ~ "Engineering",
           D102 == 3 ~ "Both",
           TRUE ~ "Other"
         ),
         Experience = case_when(
           D104 == 1 ~ "LevelLow",
           D104 == 2 ~ "LevelMedium",
           D104 == 3 ~ "LevelHigh",
           TRUE ~ "Other"
         ),
         AreaOfWork = case_when(
           D101_01 == 2 ~ "Supplier",
           D101_02 == 2 ~ "OEM",
           D101_03 == 2 ~ "Consultancy",
           D101_04 == 2 ~ "Research Institute",
           D101_05 == 2 ~ "University",
           D101_06 == 2 ~ "Government",
           D101_07 == 2 ~ "Non-Automotive",
           TRUE ~ "Other"
         )) %>%
  # select(-starts_with("D101_")) %>%
  pivot_longer(cols = starts_with("M210_"), names_to = "placementOption", values_to = "easinessRating")

modeldata$easinessRating <- factor(modeldata$easinessRating, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)



model_formula <- bf(easinessRating ~ placementOption +Experience + Expertise + AreaOfWork + (1|ID))

prior_spec <- c(
  prior(normal(0, 5), "b"), 
  prior(normal(0, 10), "Intercept"), 
  prior(cauchy(0, 2), "sd")
)


# only from priors 
fit <- brm(
  formula = model_formula,
  data = modeldata,
  family = cumulative(probit), 
 prior = prior_spec,
  chains = 4,
  sample_prior = "only",
  cores = 4,
   seed = 12345
)
pp_check(fit, type = "bars", nsamples = 250)

# Fit the model
fit <- brm(
  formula = model_formula,
  data = modeldata,
  family = cumulative(probit), 
  prior = prior_spec,
  chains = 4,
  cores = 4,
  seed = 12345
)
print(summary(fit))
plot(fit)


mcmc_trace(fit, regex_pars = "^b_") + legend_none()
rstan::check_hmc_diagnostics(eval(fit)$fit)



pp_check(fit, type = "bars", nsamples = 250)

posterior_samples <- posterior_samples(fit)

# Generating plots for all relevant coefficients
coefficients <- grep("b_", colnames(posterior_samples), value = TRUE)

plots <- lapply(coefficients, function(coef_name) {
  ggplot(posterior_samples, aes_string(x = coef_name)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste("Posterior Distribution for", gsub("b_", "", coef_name)),
         x = "Effect Size", y = "Density")
})
#for (p in plots) {
# print(p)
#}



# Extract parameter names from the fitted model
parameter_names <- colnames(posterior_samples)

# Exclude intercepts from parameters
parameters <- grep("^b_", parameter_names, value = TRUE)
parameters <- parameters[!grepl("Intercept", parameters)]

# Create the mcmc_areas plot without intercepts
p <- mcmc_areas(fit, pars = parameters, prob_outer = 0.95, prob = 0.5)

# Customize the y-axis labels
y_labels <- c("Feature Definition", "Feature Realization", "System Design",
              "Dedicated human factors team", "User-oriented teams", "Non-functional requirements team",
              "System/feature evaluation team", "Safety team", "Person/team responsible for the overall system",
              "ExperienceLevelLow", "ExperienceLevelMedium", "ExpertiseEngineering",
              "ExpertiseHumanFactors", "ExpertiseOther", "AreaOfWorkNonMAutomotive",
              "AreaOfWorkOEM", "AreaOfWorkResearchInstitute", "AreaOfWorkSupplier")

# Set y-axis labels
#p <- p + labs(y = "Parameter")
p <- p + scale_y_discrete(labels = y_labels) 
# Show the plot
print(p)







# Extract parameter names from the fitted model
parameter_names <- colnames(posterior_samples)

# Exclude intercepts from parameters
parameters <- grep("^b_", parameter_names, value = TRUE)
parameters <- parameters[!grepl("Intercept", parameters)]
print(parameters)
parameters <- c(parameters)
parameters <- rev(parameters)
# Create the mcmc_areas plot with all desired parameters
p <- mcmc_areas(fit, pars = parameters, prob_outer = 0.95, prob = 0.5) 
#+  vline_0(size = 0.3)


# Customize the y-axis labels
y_labels <- c("Feature Definition", "Feature Realization", "System Design",
              "Dedicated human factors team", "User-oriented teams", "Non-functional requirements team",
              "System/feature evaluation team", "Safety team", "Person/team responsible for the overall system",
              "ExperienceLevelLow", "ExperienceLevelMedium", "ExpertiseEngineering",
              "ExpertiseHumanFactors", "ExpertiseOther", "AreaOfWorkNonMAutomotive",
              "AreaOfWorkOEM", "AreaOfWorkResearchInstitute", "AreaOfWorkSupplier")
y_labels <- rev(y_labels)
# Modify the y-axis labels using ggplot2
p <- p + scale_y_discrete(labels = y_labels) 

# Show the plot
print(p)






# Extract parameter names from the fitted model
parameter_names <- colnames(posterior_samples)

# Filter parameters related to placementOption
placement_parameters <- grep("^b_placementOption", parameter_names, value = TRUE)

# Select only the first 9 parameters
parameters <- placement_parameters[1:9]
parameters <- rev(parameters)
# Create the mcmc_areas plot with the selected parameters
p <- mcmc_areas(fit, pars = parameters, prob_outer = 0.95, prob = 0.5) 

# Customize the y-axis labels
y_labels <- c("Feature Definition", "Feature Realization", "System Design",
              "Dedicated human factors team", "User-oriented teams", "Non-functional requirements team",
              "System/feature evaluation team", "Safety team", "Person/team responsible for the overall system")
y_labels <- rev(y_labels)
# Modify the y-axis labels using ggplot2
p <- p + scale_y_discrete(labels = y_labels) 

# Show the plot
print(p)



