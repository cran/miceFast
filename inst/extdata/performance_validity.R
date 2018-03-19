#library(devtools)
#install_github("Rcpp","RcppCore")
#install.packages("pacman)
library(pacman)

p_load(Rcpp,
       mice,
       tidyverse,
       ggthemes,
       broom,
       miceFast)

set.seed(1234)

#parameters

power = 6 # power of 10 - number of observations - should be adjusted to a computer capabilities

nr_var = 7 #CHANGE - only if you generate a bigger corr matrix:  number of variables - independent and one dependent

grs = max(c(10**(power-3),10)) # grouping variable - number of groups

iters = 10 # number of iterations for benchmarking

## generete example - data

##positive-defined correlation matrix

cors = matrix(c(1,0.6,0.7,0.4,0.4,0.5,0.35,
                NA,1,0.2,0.05,0.1,0.12,0.15,
                NA,NA,1,0.15,0.15,0.1,0.08,
                NA,NA,NA,1,0.12,0.15,0.1,
                NA,NA,NA,NA,1,0.15,0.2,
                NA,NA,NA,NA,NA,1,0.15,
                NA,NA,NA,NA,NA,NA,1),7,7,byrow = T)

cors[lower.tri(cors)] = t(cors)[lower.tri(cors)]

# automatic corr matrix - close to diagonal

#cors = stats::rWishart(100,nr_var+10,diag(nr_var))

#cors = apply(cors,1:2,mean)/(nr_var+10)

#cors

##

model = new(corrData,10,10^power,rep(0,nr_var),cors)

data_bin = model$fill("binom")
data_disc = model$fill("discrete")
data_con = model$fill("contin")

n_vars = ncol(cors)

posit_y = 1
posit_x = 2:(n_vars-2)
posit_w = n_vars-1
posit_grs = n_vars
posit_NA = n_vars+1

## NA index

index_NA = 1:nrow(data_con) %in% sample(1:nrow(data_con),10^(power-1))

fill_NA = function(v,index_NA){

  v[index_NA] = NA

  v
}

######################

group_d = floor(pnorm(data_disc[,posit_grs])*grs)
group_c = floor(pnorm(data_con[,posit_grs])*grs)
group_b = floor(pnorm(data_bin[,posit_grs])*grs)

w_d = abs(data_disc[,posit_w])
w_c = abs(data_con[,posit_w])
w_b = abs(data_bin[,posit_w])

data_disc_NA = cbind(fill_NA(data_disc[,posit_y],index_NA),data_disc[,posit_x],w_d,group_d,index_NA)
data_con_NA = cbind(fill_NA(data_con[,posit_y],index_NA),data_con[,posit_x],w_c,group_c,index_NA)
data_bin_NA = cbind(fill_NA(data_bin[,posit_y],index_NA),data_bin[,posit_x],w_b,group_b,index_NA)

colnames(data_bin_NA) = c("y",paste0("x",posit_x),"weights","group","index_NA")
colnames(data_disc_NA) = c("y",paste0("x",posit_x),"weights","group","index_NA")
colnames(data_con_NA) = c("y",paste0("x",posit_x),"weights","group","index_NA")


######################Discrete

mice.impute.lda = mice.impute.lda(data_disc[,posit_y],!index_NA,data_disc[,posit_x])

model = new(miceFast)
data = data_disc_NA[,c(posit_y,posit_x)]
model$set_data(data)
pred_miceFast =  model$impute("lda",posit_y,posit_x)

table(pred_miceFast$imputations[index_NA] ,data_disc[index_NA,posit_y])
table(as.numeric(mice.impute.lda),data_disc[index_NA,posit_y])

m1 = microbenchmark::microbenchmark(R=mice.impute.lda(data_disc[,posit_y],!index_NA,data_disc[,posit_x]),
                               miceFast={
                                 model = new(miceFast)
                                 model$set_data(data_disc_NA[,c(posit_y,posit_x)])
                                 pred_miceFast =  model$impute("lda",posit_y,posit_x)
                               },
                               times=iters)
m1

g1 = autoplot(m1,log=FALSE)+theme_economist()+ ggtitle("LDA discrete - without grouping")

ggsave("C:/Users/user/Desktop/own_R_packages/miceFast/inst/extdata/images/g1.png",g1)

### grouping variable

index_sort = sort(data_disc_NA[,posit_grs],index.return=TRUE)$ix

index_rev = sort(sort(data_disc_NA[,posit_grs],index.return=TRUE)$ix,index.return=TRUE)$ix

data_disc_NA_sort = data_disc_NA[index_sort,]

pred_Rbase = NULL
for(i in unique(data_disc_NA_sort[,posit_grs])){
    sub = data_disc_NA_sort[,posit_grs]==i
    temp = data_disc_NA_sort[sub,]
    pred = mice.impute.lda(temp[,posit_y],!temp[,posit_NA],temp[,posit_x])
    pred_Rbase = c(pred_Rbase,as.numeric(pred))
}

pred_dplyr = data_disc_NA_sort %>%
  as.data.frame() %>%
  group_by(group) %>%
  do(im = mice.impute.lda(as.matrix(.[,posit_y]),!.$index_NA,as.matrix(.[,posit_x]))) %>%
  tidy(im) %>%  arrange(group) %>% ungroup()%>% select(x) %>% unlist() %>% as.numeric()

data = data_disc_NA_sort[,c(posit_y,posit_x)]
g = data_disc_NA_sort[,posit_grs]

model = new(miceFast)
model$set_data(data)
model$set_g(g)
pred_miceFast =  model$impute("lda",posit_y,posit_x)

true_y = data_disc[index_sort,][index_NA[index_sort],posit_y]

table(pred_miceFast$imputations[as.logical(pred_miceFast$index_imp)],true_y)
table(pred_dplyr,true_y)
table(pred_Rbase,true_y)

##Performance

m2 = microbenchmark::microbenchmark(
  dplyr={
  pred_dplyr = data_disc_NA_sort %>%
    as.data.frame() %>%
    group_by(group) %>%
    do(im = mice.impute.lda(as.matrix(.[,posit_y]),!.$index_NA,as.matrix(.[,posit_x]))) %>%
    tidy(im)  %>%
    ungroup()%>%
    select(x) %>%
    unlist() %>%
    as.numeric()},
  R_base={
      pred_all = NULL
      for(i in unique(data_disc_NA_sort[,nr_var])){
        sub = data_disc_NA_sort[,posit_grs]==i
        temp = data_disc_NA_sort[sub,]
        pred = mice.impute.lda(temp[,posit_y],!temp[,posit_NA],temp[,posit_x])
        pred_Rbase = c(pred_Rbase,as.numeric(pred))}},
  miceFast={
    model = new(miceFast)
    model$set_data(data)
    model$set_g(g)
    pred_miceFast =  model$impute("lda",posit_y,posit_x)
    },
  times=iters)

m2

g2 = autoplot(m2,log=FALSE)+theme_economist()+ ggtitle("LDA discrete - with grouping")

ggsave("C:/Users/user/Desktop/own_R_packages/miceFast/inst/extdata/images/g2.png",g2)


#######################Binom

mice.impute.lda = mice.impute.lda(data_bin[,posit_y],!index_NA,data_bin[,posit_x])

data = data_bin_NA[,c(posit_y,posit_x)]

model = new(miceFast)
model$set_data(data)
pred_miceFast =  model$impute("lda",posit_y,posit_x)

table(pred_miceFast$imputations[index_NA] ,data_bin[index_NA,posit_y])
table(mice.impute.lda,data_bin[index_NA,posit_y])

m3 = microbenchmark::microbenchmark(R={mice.impute.lda = mice.impute.lda(data_bin[,posit_y],!index_NA,data_bin[,posit_x])},
                               miceFast={
                                 model = new(miceFast)
                                 model$set_data(data)
                                 pred_miceFast =  model$impute("lda",posit_y,posit_x)
                               },
                               times=iters)

m3

g3 = autoplot(m3,log=FALSE)+theme_economist()+ ggtitle("LDA binom - without grouping")

ggsave("C:/Users/user/Desktop/own_R_packages/miceFast/inst/extdata/images/g3.png",g3)


#####################Continous - LM Noise


mice.impute.norm.nob = mice.impute.norm.nob(data_con[,posit_y],!index_NA,data_con[,posit_x])

data = data_con_NA[,c(posit_y,posit_x)]

model = new(miceFast)
model$set_data(data)
pred_miceFast =  model$impute("lm_noise",posit_y,posit_x)

sum((pred_miceFast$imputations[index_NA] - data_con[index_NA,posit_y])^2)
sum((mice.impute.norm.nob - data_con[index_NA,posit_y])^2)

m4 = microbenchmark::microbenchmark(R ={mice.impute.norm.nob = mice.impute.norm.nob(data_con[,posit_y],!index_NA,data_con[,posit_x])},
                               miceFast={
                                 model = new(miceFast)
                                 model$set_data(data)
                                 pred_miceFast =  model$impute("lm_noise",posit_y,posit_x)
                               },
                               times=iters)
m4

g4 = autoplot(m4,log=FALSE)+theme_economist()+ ggtitle("linear regression noise - without grouping")

ggsave("C:/Users/user/Desktop/own_R_packages/miceFast/inst/extdata/images/g4.png",g4)



#####################Continous - LM Bayes


mice.impute.norm.bayes = mice.impute.norm(data_con[,posit_y],!index_NA,data_con[,posit_x])

data = data_con_NA[,c(posit_y,posit_x)]

model = new(miceFast)
model$set_data(data)
pred_miceFast =  model$impute("lm_bayes",posit_y,posit_x)

sum((pred_miceFast$imputations[index_NA] - data_con[index_NA,posit_y])^2)
sum((mice.impute.norm.bayes - data_con[index_NA,posit_y])^2)

m5 = microbenchmark::microbenchmark(R = mice.impute.norm(data_con[,posit_y],!index_NA,data_con[,posit_x]),
                               miceFast={
                                 model = new(miceFast)
                                 model$set_data(data)
                                 pred_miceFast =  model$impute("lm_bayes",posit_y,posit_x)
                               },
                               times=iters)
m5

g5 = autoplot(m5,log=FALSE)+theme_economist()+ ggtitle("linear regression bayes - without grouping")

ggsave("C:/Users/user/Desktop/own_R_packages/miceFast/inst/extdata/images/g5.png",g5)

#####################Continous - LM Predict


mice.impute.norm.pred = mice.impute.norm.predict(data_con[,posit_y],!index_NA,data_con[,posit_x])

data = data_con_NA[,c(posit_y,posit_x)]

model = new(miceFast)
model$set_data(data)
pred_miceFast =  model$impute("lm_pred",posit_y,posit_x)

sum((pred_miceFast$imputations[index_NA] - data_con[index_NA,posit_y])^2)
sum((mice.impute.norm.pred - data_con[index_NA,posit_y])^2)

m6 = microbenchmark::microbenchmark(R = {
  mice.impute.norm.pred = mice.impute.norm.predict(data_con[,posit_y],!index_NA,data_con[,posit_x])
},
miceFast={
  model = new(miceFast)
  model$set_data(data)
  pred_miceFast =  model$impute("lm_pred",posit_y,posit_x)
},times=iters)

m6

g6 = autoplot(m6,log=FALSE)+theme_economist()+ ggtitle("linear regression predict - without grouping")

ggsave("C:/Users/user/Desktop/own_R_packages/miceFast/inst/extdata/images/g6.png",g6)


## grouping variable

index_sort = sort(data_con_NA[,posit_grs],index.return=TRUE)$ix

index_rev = sort(sort(data_con_NA[,posit_grs],index.return=TRUE)$ix,index.return=TRUE)$ix

data_con_NA_sort = data_con_NA[index_sort,]

pred_Rbase = NULL
for(i in unique(data_con_NA_sort[,posit_grs])){
  sub = data_con_NA_sort[,posit_grs]==i
  temp = data_con_NA_sort[sub,]
  pred = mice.impute.norm.predict(as.matrix(temp[,posit_y]),!temp[,posit_NA],as.matrix(temp[,posit_x]))
  pred_Rbase = c(pred_Rbase,pred)
}

pred_dplyr = data_con_NA_sort %>%
    as.data.frame() %>%
    group_by(group) %>%
    do(im = mice.impute.norm.predict(as.matrix(.[,posit_y]),!.$index_NA,as.matrix(.[,posit_x]))) %>%
    tidy(im)  %>% ungroup()%>% select(x) %>% unlist() %>% as.numeric()

data = cbind(data_con_NA_sort[,c(posit_y,posit_x)],1)
g = data_con_NA_sort[,posit_grs]

model = new(miceFast)
model$set_data(data)
model$set_g(g)
pred_miceFast =  model$impute("lm_pred",posit_y,c(posit_x,max(posit_x)+1))

true_y = data_con[index_sort,][index_NA[index_sort],posit_y]

sum((pred_miceFast$imputations[as.logical(pred_miceFast$index_imputed)]-true_y)^2)
sum((pred_dplyr-true_y)^2)
sum((pred_Rbase-true_y)^2)

##Performance

m7 = microbenchmark::microbenchmark(
  dplyr={
    pred_dplyr = data_con_NA_sort %>%
      as.data.frame() %>%
      group_by(group) %>%
      do(im = mice.impute.norm.predict(as.matrix(.[,posit_y]),!.$index_NA,as.matrix(.[,posit_x]))) %>%
      tidy(im)  %>%
      ungroup()%>%
      select(x) %>%
      unlist() %>%
      as.numeric()
    },
  R_base={
    pred_Rbase = NULL
    for(i in unique(data_con_NA_sort[,posit_grs])){
      sub = data_con_NA_sort[,posit_grs]==i
      temp = data_con_NA_sort[sub,]
      pred = mice.impute.norm.predict(as.matrix(temp[,posit_y]),!temp[,posit_NA],as.matrix(temp[,posit_x]))
      pred_Rbase = c(pred_Rbase,pred)
    }
    },
  miceFast={
    model = new(miceFast)
    model$set_data(data)
    model$set_g(g)
    pred_miceFast =  model$impute("lm_pred",posit_y,posit_x)},
  times=iters)

m7

g7 = autoplot(m7,log=FALSE)+
  theme_economist()+
  ggtitle("linear regression predict - with grouping")

ggsave("C:/Users/user/Desktop/own_R_packages/miceFast/inst/extdata/images/g7.png",g7)
