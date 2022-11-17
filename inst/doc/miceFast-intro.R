## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

## ---- echo=TRUE,message=FALSE,warning=FALSE-----------------------------------
pkgs <- c("miceFast", "mice", "ggplot2", "dplyr", "data.table")
inst <- lapply(pkgs, library, character.only = TRUE)

## ----echo=TRUE----------------------------------------------------------------
set.seed(123456)

## ----eval=FALSE,echo=TRUE-----------------------------------------------------
#  system.file("extdata", "performance_validity.R", package = "miceFast")

## ----eval=FALSE,echo=TRUE-----------------------------------------------------
#  system.file("extdata", "images", package = "miceFast")

## ----echo=TRUE----------------------------------------------------------------
# airquality dataset with additional variables
data(air_miss)

## ---- echo=TRUE---------------------------------------------------------------
upset_NA(air_miss, 6)

## ----echo=TRUE----------------------------------------------------------------
# VIF - values bigger than 10 (around) suggest that there might be a collinearity problem.
# VIF is high for Solar.R and x_character which is obvious - x_character is a factor version of numeric Solar.R
air_miss %>%
  do(vifs = VIF(.,
    posit_y = "Ozone",
    posit_x = c(
      "Solar.R",
      "Wind",
      "Temp",
      "x_character",
      "Day",
      "weights",
      "groups"
    )
  )) %>%
  unlist()

# IMPUTATIONS
air_miss <- air_miss %>%
  # Imputations with a grouping option (models are separately assessed for each group)
  # taking into account provided weights
  group_by(groups) %>%
  do(mutate(., Solar_R_imp = fill_NA(
    x = .,
    model = "lm_pred",
    posit_y = "Solar.R",
    posit_x = c("Wind", "Temp", "Intercept"),
    w = .[["weights"]]
  ))) %>%
  ungroup() %>%
  # Imputations - discrete variable
  mutate(x_character_imp = fill_NA(
    x = .,
    model = "lda",
    posit_y = "x_character",
    posit_x = c("Wind", "Temp")
  )) %>%
  # logreg was used because almost log-normal distribution of Ozone
  # imputations around mean
  mutate(Ozone_imp1 = fill_NA(
    x = .,
    model = "lm_bayes",
    posit_y = "Ozone",
    posit_x = c("Intercept"),
    logreg = TRUE
  )) %>%
  # imputations using positions - Intercept, Temp
  mutate(Ozone_imp2 = fill_NA(
    x = .,
    model = "lm_bayes",
    posit_y = 1,
    posit_x = c(4, 6),
    logreg = TRUE
  )) %>%
  # multiple imputations (average of x30 imputations)
  # with a factor independent variable, weights and logreg options
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
  do(mutate(., Ozone_imp5 = fill_NA(
    x = .,
    model = "lm_pred",
    posit_y = "Ozone",
    posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
    w = .[["weights"]],
    logreg = TRUE
  ))) %>%
  do(mutate(., Ozone_imp6 = fill_NA_N(
    x = .,
    model = "pmm",
    posit_y = "Ozone",
    posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
    w = .[["weights"]],
    logreg = TRUE,
    k = 20
  ))) %>%
  ungroup() %>%
  # Average of a few methods
  mutate(Ozone_imp_mix = rowMeans(select(., starts_with("Ozone_imp")))) %>%
  # Protecting against collinearity or low number of observations - across small groups
  # Be carful when using a data.table grouping option
  # because of lack of protection against collinearity or low number of observations.
  # There could be used a tryCatch(fill_NA(...),error=function(e) return(...))
  group_by(groups) %>%
  do(mutate(., Ozone_chac_imp = tryCatch(
    fill_NA(
      x = .,
      model = "lda",
      posit_y = "Ozone_chac",
      posit_x = c("Intercept", "Month", "Day", "Temp", "x_character_imp"),
      w = .[["weights"]]
    ),
    error = function(e) .[["Ozone_chac"]]
  ))) %>%
  ungroup()

## ----echo=TRUE----------------------------------------------------------------
# Distribution of imputations vs Distribution of initial data
compare_imp(air_miss, origin = "Ozone", target = "Ozone_imp_mix")
# or
compare_imp(air_miss, origin = "Ozone", target = c("Ozone_imp2", "Ozone_imp_mix"))

## ----echo=TRUE----------------------------------------------------------------
data(air_miss)
setDT(air_miss)
# VIF - values bigger than 10 (around) suggest that there might be a collinearity problem.
# VIF is high for Solar.R and x_character which is obvious - x_character is a factor version of numeric Solar.R
air_miss[, .(VIF(.SD,
  posit_y = "Ozone",
  posit_x = c(
    "Solar.R",
    "Wind",
    "Temp",
    "x_character",
    "Day",
    "weights",
    "groups"
  )
))]

# IMPUTATIONS
# Imputations with a grouping option (models are separately assessed for each group)
# taking into account provided weights
air_miss[, Solar_R_imp := fill_NA_N(
  x = .SD,
  model = "lm_bayes",
  posit_y = "Solar.R",
  posit_x = c("Wind", "Temp", "Intercept"),
  w = .SD[["weights"]],
  k = 100
), by = .(groups)] %>%
  # Imputations - discrete variable
  .[, x_character_imp := fill_NA(
    x = .SD,
    model = "lda",
    posit_y = "x_character",
    posit_x = c("Wind", "Temp", "groups")
  )] %>%
  # logreg was used because almost log-normal distribution of Ozone
  # imputations around mean
  .[, Ozone_imp1 := fill_NA(
    x = .SD,
    model = "lm_bayes",
    posit_y = "Ozone",
    posit_x = c("Intercept"),
    logreg = TRUE
  )] %>%
  # imputations using positions - Intercept, Temp
  .[, Ozone_imp2 := fill_NA(
    x = .SD,
    model = "lm_bayes",
    posit_y = 1,
    posit_x = c(4, 6),
    logreg = TRUE
  )] %>%
  # model with a factor independent variable
  # multiple imputations (average of x30 imputations)
  # with a factor independent variable, weights and logreg options
  .[, Ozone_imp3 := fill_NA_N(
    x = .SD,
    model = "lm_noise",
    posit_y = "Ozone",
    posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
    w = .SD[["weights"]],
    logreg = TRUE,
    k = 30
  )] %>%
  .[, Ozone_imp4 := fill_NA_N(
    x = .SD,
    model = "lm_bayes",
    posit_y = "Ozone",
    posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
    w = .SD[["weights"]],
    logreg = TRUE,
    k = 30
  )] %>%
  .[, Ozone_imp5 := fill_NA(
    x = .SD,
    model = "lm_pred",
    posit_y = "Ozone",
    posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
    w = .SD[["weights"]],
    logreg = TRUE
  ), .(groups)] %>%
  .[, Ozone_imp6 := fill_NA_N(
    x = .SD,
    model = "pmm",
    posit_y = "Ozone",
    posit_x = c("Intercept", "x_character_imp", "Wind", "Temp"),
    w = .SD[["weights"]],
    logreg = TRUE,
    k = 10
  ), .(groups)] %>%
  # Average of a few methods
  .[, Ozone_imp_mix := apply(.SD, 1, mean), .SDcols = Ozone_imp1:Ozone_imp6] %>%
  # Protecting against collinearity or low number of observations - across small groups
  # Be careful when using a data.table grouping option
  # because of lack of protection against collinearity or low number of observations.
  # There could be used a tryCatch(fill_NA(...),error=function(e) return(...))

  .[, Ozone_chac_imp := tryCatch(
    fill_NA(
      x = .SD,
      model = "lda",
      posit_y = "Ozone_chac",
      posit_x = c("Intercept", "Month", "Day", "Temp", "x_character_imp"),
      w = .SD[["weights"]]
    ),
    error = function(e) .SD[["Ozone_chac"]]
  ), .(groups)]

## ----echo=TRUE----------------------------------------------------------------
# install.packages("mice")
data <- cbind(as.matrix(mice::nhanes), intercept = 1, index = 1:nrow(mice::nhanes))
model <- new(miceFast)
model$set_data(data) # providing data by a reference
model$get_ridge()
model$update_var(2, model$impute("lm_pred", 2, 5)$imputations)
# OR not recommended
# data[,2] = model$impute("lm_pred",2,5)$imputations
# model$set_data(data) #Updating the object

model$update_var(3, model$impute("lda", 3, c(1, 2))$imputations)

# Old slow syntax model$update_var(4,rowMeans(sapply(1:10,function(x) model$impute("lm_bayes",4,c(1,2,3))$imputations)))
# New syntax - impute_N
model$update_var(4, model$impute_N("lm_bayes", 4, c(1, 2, 3), 10)$imputations)

# When working with 'Big Data'
# it is recommended to occasionally manually invoke a garbage collector `gc()`

# Be careful with `update_var` because of the permanent update at the object and data
# That is why `update_var` could be used only ones for a certain column
# check which variables was updated - inside the object
model$which_updated()
head(model$get_data(), 3)
head(data, 3)
head(mice::nhanes, 3)
rm(model)

## ----echo=TRUE----------------------------------------------------------------
data <- cbind(as.matrix(airquality[, -5]), intercept = 1, index = 1:nrow(airquality))
weights <- rgamma(nrow(data), 3, 3) # a numeric vector - positive values
groups <- as.numeric(airquality[, 5]) # a numeric vector not integers - positive values - sorted increasingly

model <- new(miceFast)
model$set_data(data) # providing data by a reference
model$set_w(weights) # providing by a reference
model$set_g(groups) # providing by a reference

# impute adapt to provided parameters like w or g
# Simple mean - permanent imputation at the object and data
model$update_var(1, model$impute("lm_pred", 1, c(6))$imputations)

model$update_var(2, model$impute_N("lm_bayes", 2, c(1, 3, 4, 5, 6), 10)$imputations)

# Printing data and retrieving an old order
head(cbind(model$get_data(), model$get_g(), model$get_w())[order(model$get_index()), ], 4)

head(airquality, 3)

head(cbind(model$get_data(), model$get_g(), model$get_w()), 3)

head(cbind(data, groups, weights), 3)

rm(model)

## ----echo=TRUE----------------------------------------------------------------
data <- cbind(as.matrix(airquality[, -5]), intercept = 1, index = 1:nrow(airquality))
weights <- rgamma(nrow(data), 3, 3) # a numeric vector - positive values
# groups = as.numeric(airquality[,5]) # a numeric vector not integers - positive values
groups <- as.numeric(sample(1:8, nrow(data), replace = T)) # a numeric vector not integers - positive values

model <- new(miceFast)
model$set_data(data) # providing by a reference
model$set_w(weights) # providing by a reference
model$set_g(groups) # providing by a reference
# impute adapt to provided parmaters like w or g
# Warning - if data is not sorted increasingly by the g then it would be done automatically
# during a first imputation
# Simple mean - permanent imputation at the object and data
model$update_var(1, model$impute("lm_pred", 1, 6)$imputations)

model$update_var(2, model$impute_N("lm_bayes", 2, c(1, 3, 4, 5, 6), 10)$imputations)

# Printing data and retrieving an old order
head(cbind(model$get_data(), model$get_g(), model$get_w())[order(model$get_index()), ], 4)

head(airquality, 4)

head(cbind(model$get_data(), model$get_g(), model$get_w()), 4) # is ordered by g

head(cbind(data, groups, weights), 4) # is sorted by g cause we provide data by a reference

rm(model)

## ----echo=TRUE----------------------------------------------------------------
# str(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)
mtcars_mat <- model.matrix.lm(~., mtcars, na.action = "na.pass")
# str(mtcars_mat)

## ----echo=TRUE----------------------------------------------------------------
airquality2 <- airquality
airquality2$Temp2 <- airquality2$Temp**2
airquality2$Month <- factor(airquality2$Month)

# car::vif(lm(Ozone ~ ., data = airquality2))

## ----echo=TRUE----------------------------------------------------------------
data_DT <- data.table(airquality2)
data_DT[, .(vifs = VIF(
  x = .SD,
  posit_y = "Ozone",
  posit_x = c("Solar.R", "Wind", "Temp", "Month", "Day", "Temp2"), correct = FALSE
))]

data_DT[, .(vifs = VIF(
  x = .SD,
  posit_y = 1,
  posit_x = c(2, 3, 4, 5, 6, 7), correct = TRUE
))]

