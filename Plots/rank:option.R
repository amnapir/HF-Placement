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

# Load the data
data <- read_sav("Untitled26.sav")

# Recode missing values (-9) to NA
data <- data %>%
  mutate(across(starts_with("M210_"), ~if_else(. == -9, NA_real_, .))) %>%
  mutate(across(starts_with("M201_"), ~if_else(. == -9, NA_real_, .)))

# Define factor levels
data$D104 <- factor(data$D104, levels = c("1", "2", "3"), ordered = TRUE)
data$D102 <- factor(data$D102, levels = c("1", "2", "3", "4"), ordered = TRUE)

# Create new variables based on existing ones
data <- data %>%
  mutate(Expertise = case_when(
    D102 == 1 ~ "Human Factors",
    D102 == 2 ~ "Engineering",
    D102 == 3 ~ "Both",
    TRUE ~ "Other"
  ),
    Dev_Struc = case_when(
      I004_01 >= 1 & I004_01 <= 35 ~ "Waterfall",
      I004_01 >= 36 & I004_01 <= 70 ~ "Hybrid",
      I004_01 >= 71 & I004_01 <= 100 ~ "Agile",
      TRUE ~ "NA"
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
  pivot_longer(cols = starts_with("M201_"), names_to = "placementOption", values_to = "ranking")


# Define factor level for placementOption
data$ranking <- factor(data$ranking, levels = c("1", "2", "3", "4", "5", "6","7", "8","9", "10"), ordered = TRUE)

# Model formula
model_formula <- bf(ranking ~placementOption+ Experience + Expertise + AreaOfWork +Dev_Struc + (1|ID))

# Prior specification
prior_spec <- c(
  prior(normal(0, 5), "b"), 
  prior(normal(0, 10), "Intercept"), 
  prior(cauchy(0, 2), "sd")
)

data <- data %>%
  mutate(across(starts_with("M210_"), ~ haven::as_factor(.)),
         across(starts_with("M201_"), ~ haven::as_factor(.)))

# Fit the model
fit <- brm(
  formula = model_formula,
  data = data,
  family = cumulative(probit), 
  prior = prior_spec,
  chains = 4,
  cores = 4,
  seed = 12345,
)

# Summary of the model
print(summary(fit))

# Plot the model
plot(fit)

# Posterior predictive checks
pp_check(fit, type = "bars", nsamples = 250)


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
parameters <- rev(parameters)
# Customize the y-axis labels
y_labels <- c("Feature Definition", "Feature Realization", "System Design",
              "Dedicated human factors team", "User-oriented teams", "Non-functional requirements team",
              "System/feature evaluation team", "Safety team", "Person/team responsible for the overall system",
              "ExperienceLevelLow", "ExperienceLevelMedium", "ExpertiseEngineering",
              "ExpertiseHumanFactors", "ExpertiseOther", "AreaOfWorkNonMAutomotive",
              "AreaOfWorkOEM", "AreaOfWorkResearchInstitute", "AreaOfWorkSupplier")
y_labels <- rev(y_labels)
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






#conditional effects 
ce <- conditional_effects(fit, categorical = TRUE)
plot(ce, plot = FALSE)[[1]] + 
  scale_fill_colorblind(labels = c(seq(1,10))) + 
  scale_color_colorblind(labels = c(seq(1,10))) +
  xlab("Priority (scaled)") + 
  theme_tufte(base_size = 18) + 
  theme(legend.position = "none")

plot(ce, plot = FALSE)[[2]] + 
  scale_fill_colorblind(labels = c(seq(1,10))) + 
  scale_color_colorblind(labels = c(seq(1,10))) +
  xlab("Priority (scaled)") + 
  theme_tufte(base_size = 18) + 
  theme(legend.position = "none")

plot(ce, plot = FALSE)[[3]] + 
  scale_fill_colorblind(labels = c(seq(1,10))) + 
  scale_color_colorblind(labels = c(seq(1,10))) +
  xlab("Priority (scaled)") + 
  theme_tufte(base_size = 18) + 
  theme(legend.position = "none")




ce <- conditional_effects(fit, categorical = TRUE)
p <- conditional_effects(fit, effects = "Experience", categorical = TRUE)
p_crit <- plot(p)[[1]] +  # Removed plot = FALSE
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Experience") +
  ylab("") +
  scale_x_continuous(breaks=seq(0,1))

ce <- conditional_effects(fit, categorical = TRUE)
p <- conditional_effects(fit, effects = "placementOption", categorical = TRUE)
p_crit <- plot(p)[[1]] +  # Removed plot = FALSE
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("placementOption") +
  ylab("") +
  scale_x_continuous(breaks=seq(0,1))


ce <- conditional_effects(fit, categorical = TRUE)
p <- conditional_effects(fit, effects = "Expertise", categorical = TRUE)
p_crit <- plot(p)[[1]] +  # Removed plot = FALSE
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Criticality") +
  ylab("") +
  scale_x_continuous(breaks=seq(0,1))



