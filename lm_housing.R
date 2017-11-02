#***************************************
#PROJECT DESCRIPTION:
#***************************************

# the goal is to forecast the median house values based on 
#its relations with the other predictor variables.The multiple regression analysis
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
#*******************************

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
pairs.panels(Boston[,1:14])

#medv (median house value) is the target variable.
#medv is correlated with predictor variables lstat, rm and age


#Plot medv vs lstat, rm and age with least squares regression lines to check the relationship

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

plot(Boston$medv, Boston$age, pch=18, col=3,
     xlab = "ownership prior to 1940", 
     ylab = "median house value",
     main = "house value and ownership prior to 1940")
abline(lm.fit, col="red", lwd=3)


#Remake the pair plot using only the variables with significant correlations vs medv

#Get the column numbers of the variables 
install.packages("fastmatch")
fmatch("lstat", names(Boston))
fmatch("rm", names(Boston))
fmatch("age", names(Boston))

library(psych)
pairs.panels(Boston[,c(13,6,7,14)])



#******************************************
#STEP 3 : BUILD THE MODEL
#******************************************

#Multiple linear regression with all predictors
lm.fit_1<-lm(medv~., data=Boston)
summary(lm.fit_1)

#setup model with only lstat variable
lm.fit_2<-lm(medv~lstat, data=Boston)
summary(lm.fit_2)

#setup model with only rm variable
lm.fit_3<-lm(medv~rm, data=Boston)
summary(lm.fit_3)

#setup model with only age variable
lm.fit_4<-lm(medv~age, data=Boston)
summary(lm.fit_4)


