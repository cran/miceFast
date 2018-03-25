## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

## ----echo=TRUE-----------------------------------------------------------
library(miceFast)
set.seed(123456)

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

head(mice::nhanes)

## ----echo=TRUE-----------------------------------------------------------
data = cbind(as.matrix(airquality[,-5]),intercept=1,index=1:nrow(airquality))
weights = rgamma(nrow(data),3,3) # a numeric vector - positive values
groups = as.numeric(airquality[,5]) # a numeric vector not integers - positive values - sorted increasingly

model = new(miceFast)
model$set_data(data) # providing data by a reference
model$set_w(weights)
model$set_g(groups)
#impute adapt to provided parmaters like w or g
#Warning - if data is not sorted increasingly by the g then it would be done automatically 
#during a first imputation
#Simple mean - permanent imputation at the object and data
model$update_var(1,model$impute("lm_pred",1,c(6))$imputations)

model$update_var(2,rowMeans(sapply(1:10,function(x) 
  model$impute("lm_bayes",2,c(1,3,4,5,6))$imputations))
  )
#Printing data and retrieving an old order
head(cbind(model$get_data(),model$get_g(),model$get_w())[order(model$get_index()),],4)

head(airquality,4)

head(cbind(model$get_data(),model$get_g(),model$get_w()),4)

head(cbind(data,groups,weights),4)

rm(model)


## ----echo=TRUE-----------------------------------------------------------
data = cbind(as.matrix(airquality[,-5]),intercept=1,index=1:nrow(airquality))
weights = rgamma(nrow(data),3,3) # a numeric vector - positive values
#groups = as.numeric(airquality[,5]) # a numeric vector not integers - positive values
groups = as.numeric(sample(1:3,nrow(data),replace=T)) # a numeric vector not integers - positive values

model = new(miceFast)
model$set_data(data) # providing data by a reference
model$set_w(weights)
model$set_g(groups)
#impute adapt to provided parmaters like w or g
#Warning - if data is not sorted increasingly by the g then it would be done automatically 
#during a first imputation
#Simple mean - permanent imputation at the object and data
model$update_var(1,model$impute("lm_pred",1,c(6))$imputations)

model$update_var(2,rowMeans(sapply(1:10,function(x) 
  model$impute("lm_bayes",2,c(1,3,4,5,6))$imputations))
  )
#Printing data and retrieving an old order
head(cbind(model$get_data(),model$get_g(),model$get_w())[order(model$get_index()),],4)

head(airquality,4)

head(cbind(model$get_data(),model$get_g(),model$get_w()),4) #is ordered by g

head(cbind(data,groups,weights),4) #is sorted by g cause we provide data by a reference

rm(model)


