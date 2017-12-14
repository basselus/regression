x_1<-rnorm(1000, 5, 7)
hist(x_1, col = "blue")

true_error<-rnorm(1000,0,2)
true_beta_0<-1.1
true_beta_1<- -8.2

y<-true_beta_0+true_beta_1*x_1+true_error

hist(y, col = "lightblue")

#*********************************************************************
#Build a regression model and see that it covers the true values of y
#*********************************************************************

plot(x_1, y, pch=20, col="green")

lm_2<-lm(x_1~y)
summary(lm_2)

#******************************************************************
#Verification of the true parameters truebetaone and truebetazero
#******************************************************************

truebetaone=cov(x_1,y)/var(x_1)
truebetazero=mean(y)-(truebetaone*mean(x_1))


