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


#******************************************
#STEP 2 : EXPLORATORY ANALYSIS
#******************************************

library(psych)
pairs.panels(Boston[,1:14])

#medv (median house value) is the target variable.
#medv is correlated with predictor variables lstat, rm and age

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
