setwd("C:/Users/yiqin/Dropbox/UCD/18Winter/452 Machine Learning/assignemnt")

###################################################################################################
#####1.1. Try doing several runs of the linear and k-NN code in that section, comparing results.###
###################################################################################################

#install.packages("freqparcoord")
library(freqparcoord)
#install.packages("regtools")
library(regtools)
data(mlb)

# A function help you divide data set into train and validation based on p propotion 
xvalpart = function (data,p){
  # proportion p go to train dataset
  n = nrow(data)
  ntrain = round(p*n) #num of records in training
  trainidxs = sample(1:n,ntrain,replace = FALSE) #get the index of training
  list (train = data[trainidxs,], valid = data[-trainidxs,])
}

#A function to return the mean absolute error for a linear model on validation set 
xvallm = function (data, ycol, predvars, p, meanabs = TRUE){
  #ycol: column number of resp. var
  #predvars: column number of predictors
  #p: proportion of trainnign data set
  #meanabs: if true, the mean absolute error, othersise, a R list containnign pred., real Y
  
  
  #use xvalpart function to get train and validation 
  tmp = xvalpart(data,p) 
  train = tmp$train
  valid = tmp$valid
  
  #fit model to trainning data
  trainy = train[,ycol]
  trainpreds = train[,predvars]
  trainpreds = as.matrix(trainpreds)
  lmout = lm(trainy~trainpreds)
  #apply fitted model to validation data
  validpreds = as.matrix(valid[,predvars])
  predy = cbind(1,validpreds) %*% coef (lmout)
  realy = valid[,ycol]
  
  if (meanabs) return (mean(abs(predy-realy)))
  list (predy = predy, realy = realy)
}

xvallm(mlb,5,c(4,6),2/3)


xvalknn = function (data, ycol, predvars, k, p, meanabs = TRUE) {
  data = data[,c(predvars,ycol)]
  ycol = length (predvars) + 1
  
  #use xvalpart function to get train and validation 
  tmp = xvalpart(data,p)
  train = tmp$train
  valid = tmp$valid  
  
  xd = preprocessx(train[,-ycol],k)
  kout = knnest(train[,ycol],xd,k)
  predy = predict(kout,valid[,-ycol],TRUE)
  realy = valid[,ycol]

  if (meanabs) return (mean(abs(predy-realy)))
  list (predy = predy, realy = realy)  
  
}
set.seed(9999)
xvalknn(mlb,5,c(4,6),25,2/3)


###################################################################################################
####1.2. include interaction terms for age and gender, and age2 and gender. #######################
####estimated effect of being female, for a 32-year-old person with a Master's degree.#############
###################################################################################################
data (prgeng)
prgeng$age2 = prgeng$age^2
edu = prgeng$educ
prgeng$ms = as.integer(edu == 14) 
prgeng$phd = as.integer(edu == 16) 
prgeng$fem = prgeng$sex - 1
prgeng$agefem = prgeng$age*prgeng$fem #interactive terms for age and gender
prgeng$age2fem = prgeng$age2*prgeng$fem #interactive terms for age2 and gender


model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + agefem + age2fem, data = prgeng)
summary(model)
record = data.frame(age = 32, age2 = 32^2, wkswrkd = 52, ms = 1, phd = 0, fem = 1, agefem = 32*1, age2fem = 32^2*1)
predict(model,newdata = record)
# Wageinc for a 32-year-old female with a Masters degree is 69086.59 


#####################################################################################
####1.3. Use lm() to forma prediction equation for density from the other variables##
#####################################################################################
bodyfat = read.csv("bodyfat.csv")
lm_model = lm(density ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data = bodyfat)
summary(lm_model)
# Indirect methods can be applied in this case because some of variables are not significant.


####################################################################################################
########1.4. relates the overall mean height of people and the gender-specic mean heights.#########
#relates the overall proportion of people taller than 70 inches to the gender-specificc proportions#
####################################################################################################

# The overall height of people is equal to the weighted average of female and male's mean heights.
# The weighted average proportion of female and male's mean heights are taller than 70 inches



######################################################################################
####2.1. Form an approximate 95% confidence interval for ??6 and ??6+??7 in the model####
######################################################################################
data (prgeng)
prgeng$age2 = prgeng$age^2
edu = prgeng$educ
prgeng$ms = as.integer(edu == 14) 
prgeng$phd = as.integer(edu == 16) 
prgeng$fem = prgeng$sex - 1
prgeng$agefem = prgeng$age*prgeng$fem #interactive terms for age and gender
prgeng$age2fem = prgeng$age2*prgeng$fem #interactive terms for age2 and gender
model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + agefem + age2fem, data = prgeng)
model_summ = summary(model)

# confident intervals
beta6 = model_summ$coefficients['fem',]
beta7 = model_summ$coefficients['agefem',]
t_value = qt(0.975, nrow(prgeng)-1)
beta6_h = beta6[1] + t_value*beta6[2]
beta6_l = beta6[1] - t_value*beta6[2]
beta7_h = beta7[1] + t_value*beta7[2]
beta7_l = beta7[1] - t_value*beta7[2]
print(paste0("95% confidence interval for beta6: (",beta6_l,', ', beta6_h,')'))
print(paste0("95% confidence interval for beta6 + beta7: (",beta6_l+beta7_l,', ', beta6_h+beta7_h,')'))


##############################################################################
####2.2 95% confidence interval for the difference betweenthe coefficients####
##############################################################################
day = read.csv('day.csv')
day$temp2 <- (day$temp)^2
day$clearday <- as.integer(day$weathersit == 1)
model_bike <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = day)
model_bike_summ <- summary(model_bike)
t_value <- qt(0.975, nrow(day)-1)
yr <- model_bike_summ$coefficients['yr',]
yr_l <- yr[1] - t_value * yr[2]
yr_h <- yr[1] + t_value * yr[2]
print(paste0("95% confidence interval: (",yr_l,', ', yr_h,')'))


##################################################################################################################
####2.3 why each Di is (k????1)-variate normal, and derive matrix expressions for the mean vector and covarianceat####
##################################################################################################################


##################################################
####2.4 Confirm rho^2 = 0.5 through derivation####
##################################################