#***************************************
#PROJECT DESCRIPTION:
#***************************************

# the goal is to forecast the median house values based on 
#their relations with the other predictor variables.The multiple regression analysis
#will estimate the impact of the latter variables on the median house values.
#we will first build a regression function to estimate the values of the independant variables.
#second we will use the lm funcion from the stats package included by default in R.

#******************************************
#STEP 1 : LOAD DATASET AND EXPLORE DATASET
#******************************************

library(ISLR)
library(car)
library(MASS)
data("Boston")
str(Boston)
summary(Boston)
names(Boston)

#COLUMN NAMES:

#crim:per capita crime rate by town.

#zn:proportion of residential land zoned for lots over 25,000 sq.ft.

#indus:proportion of non-retail business acres per town.

#chas:Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).

#nox:nitrogen oxides concentration (parts per 10 million).

#rm:average number of rooms per dwelling.

#age:proportion of owner-occupied units built prior to 1940.

#dis:weighted mean of distances to five Boston employment centres.

#rad:index of accessibility to radial highways.

#tax:full-value property-tax rate per \$10,000.

#ptratio:pupil-teacher ratio by town.

#black:1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.

#lstat:lower status of the population (percent).

#medv:median value of owner-occupied homes in \$1000s.

#********************************
 # Build the regression function
#********************************

# The function takes the parameters x and y, returning a vector of b (beta coefficients)

reg<- function (x,y){
  
  x<-as.matrix(x)
  x<-cbind(Intercept=1, x)
  b<-solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b)<- "estimate"
  print(b)

  }


#**********************************************************
# use the reg function to build a multiple regression model
#**********************************************************

reg(y=Boston$medv, x=Boston[1:13])


#******************************************
#STEP 2 : EXPLORATORY ANALYSIS
#******************************************

library(psych)
round(corrs<-cor(Boston), 2)

#Build the pair plot using only the variables with significant correlations vs medv

#Get column numbers of the variables 
install.packages("fastmatch")
fmatch("lstat", names(Boston))
fmatch("rm", names(Boston))


library(psych)
pairs.panels(Boston[,c(13,6,14)])


#medv (median house value) is the target variable.
#medv is correlated with predictor variables lstat and rm 


#Plot medv vs lstat, and rm with least squares regression lines to check the relationship

plot(Boston$medv, Boston$lstat, pch=18, col=3,
     xlab = "lower status of the population", 
     ylab = "median house value",
     main = "house value and wealth status")
abline(lm.fit, col="red", lwd=3)

plot(Boston$medv, Boston$rm, pch=18, col=3,
     xlab = "avg room numbers", 
     ylab = "median house value",
     main = "house value and avg room numbers")
abline(lm.fit, col="red", lwd=3)



#****************************************************
#STEP 3 : BUILD THE MODEL AND EXPLORE THE PARAMETERS
#****************************************************

#Multiple linear regression with all predictors
lm.fit_all<-lm(medv~., data=Boston)
summary(lm.fit_all)
names(lm.fit_all)
coef(lm.fit_all)

#Predict median house values (medv) for given values of lstat and rm)

#fit a model with only lstat variable
lm.fit_2<-lm(medv~lstat, data=Boston)
summary(lm.fit_2)
names(lm.fit_2)
coef(lm.fit_2)

#fit a model with only rm variable
lm.fit_3<-lm(medv~rm, data=Boston)
summary(lm.fit_3)
names(lm.fit_3)
coef(lm.fit_3)


predict(lm.fit_2, data.frame(lstat=c(15, 45)), interval = "confidence")
#Comment : There is 95% confidence that when lstat=15 medv (median house value) is between
# 19.7  and 20.9

predict(lm.fit_2, data.frame(lstat=c(15, 45)), interval = "prediction")
#Comment : 95% prediction interval is between 8.07 and 32.5  for median house value
#when lstat=15

predict(lm.fit_3, data.frame(rm=c(7, 10)), interval = "confidence" )
predict(lm.fit_3, data.frame(rm=c(7, 10)), interval = "prediction" )

#********************************************
#EVALUATE MODEL PERFORMANCE
#********************************************

#try the multiple linear regression with 2 predictors age and lstat
lm.fit_m<-lm(medv~lstat+age, data = Boston)
summary(lm.fit_m)

#All variables
lm.fit_all<-lm(medv~., data=Boston)
summary(lm.fit_all)

#Only lstat variable
lm.fit_lstat<-lm(medv~lstat, data=Boston)
summary(lm.fit_lstat)

#summary statistics for Rsquared values
summary(lm.fit_m)$r.sq

#summary for RSE
summary(lm.fit_m)$sigma

### use vif() to compute variance inflation factors
### vif is part of car library
vif(lm.fit_m)

lm.fit_age<-lm(medv~age, data = Boston)
summary(lm.fit_age)

### in the last regression $age has a high p-value, meaning that age may have an interaction
#with the error term

### run regression using all predictors and excluding $age
lm.fit_noage<-lm(medv~.-age, data=Boston)
summary(lm.fit_noage)


### Verify the interaction terms (age and lstat)
summary(lm(medv ~ lstat*age , data = Boston ))
#Comment : the interaction between age and lstat does not improve the model(r-squared value is still weak)


### We try a non-linear transformation of the predictors
lm.fit_transformed <- lm(medv ~lstat + I(lstat ^2), data = Boston)
summary(lm.fit_transformed)

#Comment : ### a near Zero p-value associated with the quadratic term
### leads to an improved model

## use anova() to further quantify the extent to which quadratic
### fit is superior to the linear fit
lm.fit <- lm(medv ~ lstat, data = Boston)
anova(lm.fit, lm.fit_transformed)

#Comment : ### null hypothesis is that the two models fit data equally well
### alternative hypothesis is that the full model is superior
### F-statistic is 135 and p-value ~ 0 is clear evidence 
### that model containing the predictors lstat and lstat^2
### is superior


