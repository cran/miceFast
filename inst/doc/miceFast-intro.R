## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

## ----echo=TRUE-----------------------------------------------------------
library(miceFast)
set.seed(1234)

## ----eval=FALSE,echo=TRUE------------------------------------------------
#  system.file("extdata","performance_validity.R",package = "miceFast")

## ----eval=FALSE,echo=TRUE------------------------------------------------
#  system.file("extdata","images",package = "miceFast")

## ----echo=TRUE-----------------------------------------------------------
#install.packages("mice")

data = cbind(as.matrix(mice::nhanes),intercept=1,index=1:nrow(mice::nhanes))

model = new(miceFast)
model$set_data(data) #providing data by a reference

model$update_var(2,model$impute("lm_pred",2,5)$imputations)

#OR not recommended
#data[,2] = model$impute("lm_pred",2,5)$imputations
#model$set_data(data) #Updating the object

model$update_var(3,model$impute("lda",3,c(1,2))$imputations) 
model$update_var(4,rowMeans(sapply(1:10,function(x) 
  model$impute("lm_bayes",4,c(1,2,3))$imputations))
  )

#When working with 'Big Data'
#it is recommended to occasionally manually invoke a garbage collector `gc()`

# Be careful with `update_var` because of the permanent update at the object and data
# That is why `update_var` could be used only ones for a certain column
# check which variables was updated - inside the object
model$which_updated()

head(model$get_data())

rm(model)

## ----echo=TRUE-----------------------------------------------------------
###################################
###Model with additional parameters
###################################

data = cbind(as.matrix(airquality[,-5]),intercept=1,index=1:nrow(airquality))
weights = rgamma(nrow(data),3,3) # positive numeric values
#groups = airquality[,5] # vector of positive integers
groups = sample(1:4,nrow(data),replace=T) # vector of positive integers

model = new(miceFast)
model$set_data(data) # providing data by a reference
model$set_w(weights)
model$set_g(groups)

#if data is not sorted increasingly by g then it would be automatically done 
#during a first imputation

#impute adapt to provided parmaters like w or g
#Simple mean - permanent imputation at the object and data
model$update_var(1,model$impute("lm_pred",1,c(6))$imputations)

model$update_var(2,rowMeans(sapply(1:10,function(x) 
  model$impute("lm_bayes",2,c(1,3,4,5,6))$imputations))
  )

head(cbind(model$get_data(),model$get_g(),model$get_w())[order(model$get_index()),])

rm(model)

