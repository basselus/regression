#*******************************************************************
#*******************************************************************

#Simple linear prediction model example

#Create dataframe
size <- c(2104, 1416, 1534, 852)
price <- c(460, 232, 315, 178)
regtrain<-data.frame(size,price)
str(regtrain)

#explore data
summary(regtrain)

#visualize correlations between variables (the two variables ar highly correlated positively)
library(psych)
pairs.panels(regtrain) 

plot(size,price, col=2, pch=16,bg="yellow",bty="l",
     xlab = "size in feet", ylab = "price ($) in 1000", 
     main = "simple linear model")

#make a linear regression model of the dataframe
lm_prices<-lm(price~size, data = regtrain)
summary(lm_prices)

#uses the lm_prices' coef to plot the best line
abline(coef(lm_prices), lwd=1)
lines(size[order(size)], price[order(price)],col="blue")

#Predict the price for 3000 feet
size_3000<-data.frame(size=3000)
predict(lm_prices,size_3000)

#*******************************************************************