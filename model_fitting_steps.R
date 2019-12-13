
#********************************************************************
#*********************  INTRODUCTION ********************************

# This sample code displays the main steps and pre-requisites needed 
# to perform a simple linear regression model. Mores example and detailed 
# techniques can be found in the book
# "Doing Data Science" from Cathy O'neil.  

#********************************************************************
#********************************************************************


# recap tips for evaluation metrics
# 1- b value (positive or negative effect on the target variable)
# 2- p-value (probability of b being null)
# 3 -R squarred
# 4- cross validation (divide data between train and test (respectively 80% vs 20%), fit the model on both
# and compare MSE -do it over and over on different sample sizes)
# 5-Check if residuals follow a normal distribution

#*************************
# simulating fake datasets
#*************************

x_1 = rnorm(1000,5,7)
hist(x_1, col = "blue")
true_error = rnorm(1000,0,2)
true_beta0=1.1
true_beta1=-8.2


#*****************
# write the model
#*****************

y = true_beta0 + true_beta1*x_1 + true_error

# plot(y)

hist(y, col = "lightblue")

# plot(x_1, y)

plot(x_1, y, pch=20, col="green")

# x_1 has a negative effect on y (y decreases as X_1 increases)

#****************************************
# fit a first model depending only on x_1 
#****************************************

  mod_1=lm(y~x_1)
  plot(mod_1)
  abline(lm(y~x_1))
  summary(mod_1)
  
  #check if residuals follow a normal distribution
  hist(mod_1$residuals, col = "red")
  
  
  # Pick a x_2 variable with a gamma distribution
  x_2 = rgamma(1000, shape = 20, rate = 15)
  hist(x_2, col = "green")

#******************************************
# fit a second model including X_2 and x_1
#******************************************

  
  mod_2=lm(y~x_1+x_2)
  summary(mod_2)
  
  #check if residuals follow a normal distribution
  hist(mod_2$residuals, col="black")
  
  # the residuals of mod_2 also follow a normal distribution : proof of validity of the model

#******************************************
# fit a third model depending only on x_2
#******************************************
  
mod_3=lm(y~x_2)
abline(lm(y~x_2))
hist(mod_3$residuals, col="green")

# the residuals of mod_3 follow also a normal distribution
# display pair plots to have a global view of the interaction of variables 

library(psych)

#**********************************************
#Create the data
#**********************************************


data=data.frame(cbind(x_1,x_2,y,true_error))
dim(data)
head(data)

#check overall correlations between variables
library(psych)
pairs.panels(data[1:3])

# Only X_1 seems to have a real effect on y : there is a strong negative relationship


#***************************************************************************
# Cross validation strategy : Divide data between a training and a test set
#***************************************************************************

  #Set training set to 70% of the sample size
  smp_size=floor(0.70*nrow(data))
  
  #set the seed to make the partition reproducible
  set.seed(123)
  train_rows=sample(seq_len(nrow(data)), size = smp_size)

  trainset<-data[train_rows,]
  testset<-data[-train_rows,]
                                 
  #implement a function to calculate Mean absolute error (this can be useful when we have 
  #to predict future data based on actual dataset)
  MAE <-function(actual, predicted){
    
    means(abs(actual-predicted))

      }

  #**********************************************
  # Fit the model on the training set
  #**********************************************
  mod_train <- lm(y~x_1, data = trainset)
  summary(mod_train)  
  
  
  #**********************************************
  # Fit the model on the test set
  #**********************************************
  mod_test <- lm(y~x_1, data = testset)
  summary(mod_test)  
  
  
  
  #***************************************************************
  # Check if residuals of both models follow a normal distribution
  #***************************************************************
  
  hist(mod_train$residuals, col = "green")
  hist(mod_test$residuals, col = "blue")
  
  
  #**********************************************
  # compare MSE on both models
  #**********************************************
  
  MSE_mod_train = mean(mod_train$residuals^2)
  MSE_mod_test = mean(mod_test$residuals^2)
  
  #The mean square error of test and training sets are fairly similar.
  #The residuals follow a normal distribution
  #The conditions of a valid model are met
  #Next : check if error terms (residuals) have constant variance accross values of x
  
  
  #**********************************************
  # Creation of Z variable to include in the model
  #**********************************************
  z<-x_1^2
  data<-data.frame(cbind(data, z))
  
  #fit the new model including z and x_1
  mod_4<- lm(y~x_1+z, data = data)
  summary(mod_4)  
  pairs.panels(data[, c(1,2,3,5)])

  
  #*********************************************************************************
  # vary the sample size for model 4 and compare MSE ont test and training dataset
  #*********************************************************************************
  
  x_1 = rnorm(2000,10,20)
  x_2 = rgamma(2000, shape = 50, rate = 25)
  true_error = rnorm(2000,0,2)
  z =x_1^2
  true_beta0=1.1
  true_beta1=-8.2
  true_beta2=-15.2
  true_beta3=-30.5
  
  # Write the new model
  y = true_beta0 + true_beta1*x_1 + true_beta2*x_2 + true_error
  hist(y, col = "blue")
  
  #Check the correlation between variables
  library(psych)
  data_bis=data.frame(x_1, x_2, y, z)
  pairs.panels(data_bis)
  
  # Fit the model using all variables
  mod_5 = lm(y~x_1+x_2+z)
  summary(mod_5)
  
  # Check if residuals (errors) follow a normal distribution
  hist(mod_5$residuals, col = "lightblue")  
  
  # Split the data between train and test sets
  
  #training set will be 70% of the sample size
  smp_size_2=floor(0.70*nrow(data_bis))
  
  #set the seed to make the partition reproducible
  train_rows_2=sample(seq_len(nrow(data_bis)), size = smp_size_2)
  
  trainset_2<-data_bis[train_rows_2,]
  testset_2<-data_bis[-train_rows_2,]
  
  # Fit train and test set
  mod_train_2 = lm(y~x_1+x_2, data = trainset_2)
  mod_test_2 = lm(y~x_1+x_2, data = testset_2)
  
  #Check error terms for 2 samples (normality or not)
  hist(mod_train_2$residuals, col = "green")
  hist(mod_test_2$residuals, col = "blue")
  
  # Compare MSE of training and test set
  MSE_mod_train_2 = mean(mod_train_2$residuals^2)
  MSE_mod_test_2 = mean(mod_test_2$residuals^2)
  
  # MSE of both samples is fairly the same
  # Nullity probability of beta 0 parameter is inferorior to 5 % (pvalue)
  # R-squared value is very high (the percentage of variance explained by the model)
  # The cross validation method shows that MSE of training and test sets is fairly the same 
  # The error terms of both samples are normally distributed with mean = o
  
    



  
  
