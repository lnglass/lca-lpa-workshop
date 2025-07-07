
# LCA Workshop QRM 2025 - Kajsa Yang Hansen and Leah Glassow 
library(tidyverse)
library(janitor)
library(dplyr)
library(poLCA)
library(tidyLPA)
library(haven)
library(tibble)
library(ggplot2)
library(purrr)
library(nnet)
library(tidyr)

# Load and clean data (insert your file path below)
data <- read_sav("C:/LCA Workshop SAV/ugu2004_wellbeing_leisure_grade.sav") %>%
  clean_names()

# Example 1 - LCA

# Define variables
lca_vars <- c("lmusic", "tvmytb", "games", "sports", "read", "acting", "friends", "computer")

# Check missingness
data %>%
  dplyr::select(all_of(lca_vars)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  print()

# Prepare data
data_lca <- data %>%
  dplyr::select(all_of(lca_vars)) %>%
  drop_na()

# Model formula
f <- cbind(lmusic, tvmytb, games, sports, read, acting, friends, computer) ~ 1

# Fit models
set.seed(123)
model_2 <- poLCA(f, data_lca, nclass = 2, maxiter = 5000, graphs = FALSE)
model_3 <- poLCA(f, data_lca, nclass = 3, maxiter = 5000, graphs = FALSE)
model_4 <- poLCA(f, data_lca, nclass = 4, maxiter = 5000, graphs = FALSE)

# Model fit indices
fit_stats <- tibble(
  Classes = c(2, 3, 4),
  LogLikelihood = c(model_2$llik, model_3$llik, model_4$llik),
  BIC = c(model_2$bic, model_3$bic, model_4$bic),
  AIC = c(model_2$aic, model_3$aic, model_4$aic)
)
print(fit_stats)

# Fixed profile extraction function
extract_profile <- function(model, class_label) {
  bind_rows(lapply(names(model$probs), function(varname) {
    p <- model$probs[[varname]]
    n_classes <- nrow(p)
    map_dfr(1:n_classes, function(k) {
      # Clean column names like "Pr(1)" → 1
      response_vals <- as.numeric(gsub("Pr\\((\\d+)\\)", "\\1", colnames(p)))
      mean_score <- sum(response_vals * p[k, ])
      tibble(variable = varname,
             class = paste0(class_label, ": Class ", k),
             mean_score = mean_score)
    })
  }))
}

# Generate profiles
profiles <- bind_rows(
  extract_profile(model_2, "2-Class"),
  extract_profile(model_3, "3-Class"),
  extract_profile(model_4, "4-Class")
)

# Plot
profiles <- profiles %>%
  mutate(model = sub(":.*", "", class))

ggplot(profiles, aes(x = variable, y = mean_score, group = class, color = class)) +
  geom_line() +
  geom_point() +
  facet_wrap(~model, ncol = 1) +
  theme_minimal() +
  labs(title = "Latent Class Profiles (Split by Model)",
       y = "Estimated Mean Response",
       x = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Predict class membership using covariates 

# Convert covariates to factors (preserving SPSS value labels)
data <- data %>%
  filter(migr2 != -777) %>%                  # remove the unwanted rows
  mutate(
    sex = as_factor(sex),
    migr2 = droplevels(as_factor(migr2)),    # drop unused levels like -777
    sun2 = as_factor(sun2)
  )

# Create covariate-enhanced dataset using only complete LCA cases
# We'll filter rows in `data` to match `data_lca` (which already dropped NA in LCA vars)
data_lca_cov <- data %>%
  dplyr::filter(complete.cases(dplyr::select(., dplyr::all_of(lca_vars)))) %>%
  dplyr::select(dplyr::all_of(c(lca_vars, "sun2", "migr2", "sex"))) %>%
  tidyr::drop_na()

# Specify model formula with covariates
f_cov <- cbind(lmusic, tvmytb, games, sports, read, acting, friends, computer) ~ sun2 + migr2 + sex

# Fit the 4-class LCA model with covariates
set.seed(123)
model_3_cov <- poLCA(f_cov, data_lca_cov, nclass = 3, maxiter = 5000)

# Example 2 - LPA 

# Load and clean data (insert your file path below)
data2 <- read_sav("C:/LCA Workshop SAV/IQ_10HIGHEST.sav") %>%
  clean_names() 

# Define LPA variables
lpa_vars <- c("intrisic", "extrinsic", "attitude", "cogwb", "psywb", "socwb")

# Check missing values in LPA variables
data2 %>%
  dplyr::select(all_of(lpa_vars)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  print()

# Drop missing rows in LPA variables only
data_lpa <- data2 %>%
  drop_na(all_of(lpa_vars))

# Estimate and compare 1–4 profile solutions
set.seed(123)
models <- data_lpa %>%
  dplyr::select(all_of(lpa_vars)) %>%
  estimate_profiles(1:4, models = 1)

# View comparison summary 
compare_solutions(models)

# Complete model fit details for each model
models[[1]]$fit  # 1-class model
models[[2]]$fit  # 2-class model
models[[3]]$fit  # 3-class model
models[[4]]$fit  # 4-class model

# Extract profile data from each model
extract_profile_means <- function(model, label) {
  df <- get_data(model)
  df$profile <- factor(df$Class)
  
  df %>%
    pivot_longer(cols = all_of(lpa_vars), names_to = "variable", values_to = "value") %>%
    group_by(profile, variable) %>%
    summarise(mean_score = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(model = label,
           profile = paste0(label, ": Class ", profile)) %>%
    complete(model, profile, variable, fill = list(mean_score = NA))
}

# Apply to all models
profiles_all <- bind_rows(
  extract_profile_means(models[[2]], "2-Class"),
  extract_profile_means(models[[3]], "3-Class"),
  extract_profile_means(models[[4]], "4-Class"),
  
)

# Plot all class profiles side-by-side
ggplot(profiles_all, aes(x = variable, y = mean_score, group = profile, color = profile)) +
  geom_line() +
  geom_point() +
  facet_wrap(~model, ncol = 1) +
  theme_minimal() +
  labs(title = "Latent Profile Models: Class Mean Comparisons",
       x = "Variable",
       y = "Mean Score",
       color = "Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Predict profile membership based on covariates 
#Extract predicted class membership from the 2-class model
model_2 <- models[[2]]

#Combine predicted classes with original data
data_pred_2class <- get_data(model_2) %>%
  dplyr::rename(profile = Class) %>%
  dplyr::bind_cols(data_lpa) %>%
  dplyr::mutate(
    profile = factor(profile),       
    sex = as_factor(sex),           
    mig = as_factor(mig),
    sun = as_factor(rsrsun2)         
  ) %>%
  tidyr::drop_na(sex, mig, sun)    

#Fit multinomial logistic regression
model_cov_2class <- multinom(profile ~ sex + mig + sun, data = data_pred_2class)

# Extract summary
levels(data_pred_2class$profile)
summary_model <- summary(model_cov_2class)


# Extract coefficients and standard errors
coefs <- summary_model$coefficients
std_err <- summary_model$standard.errors

# Compute z-values and p-values
z_scores <- coefs / std_err
p_values <- 2 * (1 - pnorm(abs(z_scores)))

# Calculate odds ratios
odds_ratios <- exp(coefs)

# Combine into a tidy table
results_table <- as.data.frame(coefs)
results_table$SE <- as.vector(std_err)
results_table$P <- as.vector(p_values)
results_table$Odds_Ratio <- as.vector(odds_ratios)

# Round and print
results_table <- round(results_table, 3)
print(results_table)


# GPA as a distal outcome
# Combine profile membership with LPA input data 
distal_data <- get_data(model_2) %>%
  dplyr::rename(profile = Class) %>%
  dplyr::bind_cols(data_lpa) %>%
  dplyr::mutate(
    profile = factor(profile),
    gpa = data_lpa$gpa  
  ) %>%
  tidyr::drop_na(gpa)

# Optional: check group means
distal_data %>%
  dplyr::group_by(profile) %>%
  dplyr::summarise(mean_gpa = mean(gpa), n = dplyr::n())

# Run ANOVA
gpa_anova <- aov(gpa ~ profile, data = distal_data)
summary(gpa_anova)

