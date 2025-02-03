## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

## ---- echo=TRUE, message=FALSE, warning=FALSE---------------------------------
pkgs <- c("miceFast", "mice", "ggplot2", "dplyr", "data.table")
inst <- lapply(pkgs, library, character.only = TRUE)
set.seed(123456)

## ---- echo=TRUE---------------------------------------------------------------
data(air_miss)  # airquality with additional variables

## ---- echo=TRUE---------------------------------------------------------------
upset_NA(air_miss, 6)

## ---- echo=TRUE---------------------------------------------------------------
# VIF (Variance Inflation Factor) > ~10 suggests collinearity problems.
VIF(
  air_miss,
  posit_y = "Ozone",
  posit_x = c("Solar.R", "Wind", "Temp", "x_character",
              "Day", "weights", "groups")
)

## ---- echo=TRUE---------------------------------------------------------------
air_miss <- air_miss %>%
  # Impute with grouping and weights
  group_by(groups) %>%
  do(mutate(., 
    Solar_R_imp = fill_NA(
      x = .,
      model = "lm_pred",
      posit_y = "Solar.R",
      posit_x = c("Wind", "Temp", "Intercept"),
      w = .[["weights"]]
    )
  )) %>%
  ungroup() %>%
  # Impute a discrete variable
  mutate(x_character_imp = fill_NA(
    x = .,
    model = "lda",
    posit_y = "x_character",
    posit_x = c("Wind", "Temp")
  )) %>%
  # Log regression because Ozone is roughly log-normal
  mutate(Ozone_imp1 = fill_NA(
    x = .,
    model = "lm_bayes",
    posit_y = "Ozone",
    posit_x = c("Intercept"),
    logreg = TRUE
  )) %>%
  # Impute by position indices
  mutate(Ozone_imp2 = fill_NA(
    x = .,
    model = "lm_bayes",
    posit_y = 1,
    posit_x = c(4, 6),
    logreg = TRUE
  )) %>%
  # Imputations (mean of 30 draws)
  mutate(Ozone_imp3 = fill_NA_N(
    x = .,
    model = "lm_noise",
    posit_y = "Ozone",
    posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
    w = .[["weights"]],
    logreg = TRUE,
    k = 30
  )) %>%
  mutate(Ozone_imp4 = fill_NA_N(
    x = .,
    model = "lm_bayes",
    posit_y = "Ozone",
    posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
    w = .[["weights"]],
    logreg = TRUE,
    k = 30
  )) %>%
  group_by(groups) %>%
  # Single evaluation model
  do(mutate(.,
    Ozone_imp5 = fill_NA(
      x = .,
      model = "lm_pred",
      posit_y = "Ozone",
      posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
      w = .[["weights"]],
      logreg = TRUE
    )
  )) %>%
  do(mutate(.,
    Ozone_imp6 = fill_NA_N(
      x = .,
      model = "pmm",
      posit_y = "Ozone",
      posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
      w = .[["weights"]],
      logreg = TRUE,
      k = 20
    )
  )) %>%
  ungroup() %>%
  # Combine imputations
  mutate(Ozone_imp_mix = rowMeans(select(., starts_with("Ozone_imp")))) %>%
  # Protect against errors in small groups
  group_by(groups) %>%
  do(mutate(.,
    Ozone_chac_imp = tryCatch(
      fill_NA(
        x = .,
        model = "lda",
        posit_y = "Ozone_chac",
        posit_x = c("Intercept", "Month", "Day", "Temp", "x_character_imp"),
        w = .[["weights"]]
      ),
      error = function(e) .[["Ozone_chac"]]
    )
  )) %>%
  ungroup()

## ---- echo=TRUE---------------------------------------------------------------
# Compare original vs. imputed distributions
compare_imp(air_miss, origin = "Ozone", target = "Ozone_imp_mix")

# Or compare multiple imputation columns
compare_imp(air_miss, origin = "Ozone", target = c("Ozone_imp2", "Ozone_imp_mix"))

## ---- echo=TRUE---------------------------------------------------------------
data(air_miss)
setDT(air_miss)

## ---- echo=TRUE---------------------------------------------------------------
# Imputations
air_miss[, Solar_R_imp := fill_NA_N(
  x = .SD,
  model = "lm_bayes",
  posit_y = "Solar.R",
  posit_x = c("Wind","Temp","Intercept"),
  w = .SD[["weights"]],
  k = 100
), by = .(groups)] %>%
  .[, x_character_imp := fill_NA(
    x = .SD,
    model = "lda",
    posit_y = "x_character",
    posit_x = c("Wind","Temp","groups")
  )] %>%
  .[, Ozone_imp1 := fill_NA(
    x = .SD,
    model = "lm_bayes",
    posit_y = "Ozone",
    posit_x = c("Intercept"),
    logreg = TRUE
  )] %>%
  .[, Ozone_imp2 := fill_NA(
    x = .SD,
    model = "lm_bayes",
    posit_y = 1,
    posit_x = c(4,6),
    logreg = TRUE
  )] %>%
  .[, Ozone_imp3 := fill_NA_N(
    x = .SD,
    model = "lm_noise",
    posit_y = "Ozone",
    posit_x = c("Intercept","x_character_imp","Wind","Temp"),
    w = .SD[["weights"]],
    logreg = TRUE,
    k = 30
  )] %>%
  .[, Ozone_imp4 := fill_NA_N(
    x = .SD,
    model = "lm_bayes",
    posit_y = "Ozone",
    posit_x = c("Intercept","x_character_imp","Wind","Temp"),
    w = .SD[["weights"]],
    logreg = TRUE,
    k = 30
  )] %>%
  .[, Ozone_imp5 := fill_NA(
    x = .SD,
    model = "lm_pred",
    posit_y = "Ozone",
    posit_x = c("Intercept","x_character_imp","Wind","Temp"),
    w = .SD[["weights"]],
    logreg = TRUE
  ), by = .(groups)] %>%
  .[, Ozone_imp6 := fill_NA_N(
    x = .SD,
    model = "pmm",
    posit_y = "Ozone",
    posit_x = c("Intercept","x_character_imp","Wind","Temp"),
    w = .SD[["weights"]],
    logreg = TRUE,
    k = 10
  ), by = .(groups)] %>%
  # Average across a set of methods
  .[, Ozone_imp_mix := apply(.SD, 1, mean), .SDcols = Ozone_imp1:Ozone_imp6] %>%
  # tryCatch for small group issues or collinearity
  .[, Ozone_chac_imp := tryCatch(
    fill_NA(
      x = .SD,
      model = "lda",
      posit_y = "Ozone_chac",
      posit_x = c("Intercept","Month","Day","Temp","x_character_imp"),
      w = .SD[["weights"]]
    ),
    error = function(e) .SD[["Ozone_chac"]]
  ), by = .(groups)]

## ---- eval=FALSE--------------------------------------------------------------
#  cd <- new(corrData, nr_cat = 3, n_obs = 1000, means = c(0,1,2), cor_matrix = some_matrix)
#  generated_data <- cd$fill("discrete")

## ---- echo=TRUE---------------------------------------------------------------
data <- cbind(as.matrix(mice::nhanes), intercept = 1, index = 1:nrow(mice::nhanes))
model <- new(miceFast)
model$set_data(data)

# Single imputation, linear model
imp_result <- model$impute("lm_pred", posit_y = 2, posit_x = 5)
model$update_var(2, imp_result$imputations)

# LDA imputation for a categorical variable
model$update_var(3, model$impute("lda", 3, c(1,2))$imputations)

# Multiple imputation example
multi_imp <- model$impute_N("lm_bayes", 4, c(1,2,3), k = 10)
model$update_var(4, multi_imp$imputations)

# Check which variables were updated
model$which_updated()

# Always be cautious with 'update_var' 
# since it permanently alters the object and your data matrix.

## ---- echo=TRUE---------------------------------------------------------------
data <- cbind(as.matrix(airquality[,-5]), intercept = 1, index = 1:nrow(airquality))
weights <- rgamma(nrow(data), 3, 3)
groups <- as.numeric(airquality[,5]) 

model <- new(miceFast)
model$set_data(data)
model$set_w(weights)
model$set_g(groups)

# Impute with group-wise weighting
model$update_var(1, model$impute("lm_pred", 1, 6)$imputations)
model$update_var(2, model$impute_N("lm_bayes", 2, c(1,3,4,5,6), k = 10)$imputations)

# Inspect updated data, returning to original order via model$get_index()
head(cbind(model$get_data(), model$get_g(), model$get_w())[order(model$get_index()), ], 4)

## ---- echo=TRUE---------------------------------------------------------------
data <- cbind(as.matrix(airquality[, -5]), intercept = 1, index = 1:nrow(airquality))
weights <- rgamma(nrow(data), 3, 3)
groups <- as.numeric(sample(1:8, nrow(data), replace = TRUE))

model <- new(miceFast)
model$set_data(data)
model$set_w(weights)
model$set_g(groups)

# The data will be automatically sorted by 'groups' during the first imputation:
model$update_var(1, model$impute("lm_pred", 1, 6)$imputations)
model$update_var(2, model$impute_N("lm_bayes", 2, c(1,3,4,5,6), 10)$imputations)

head(cbind(model$get_data(), model$get_g(), model$get_w())[order(model$get_index()), ], 4)

