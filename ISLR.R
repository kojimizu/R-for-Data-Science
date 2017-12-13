
http://www-bcf.usc.edu/~gareth/ISL/Chapter%204%20Lab.txt
http://blog.princehonest.com/stat-learning/
  

# ISLR Review
# Chap2: statistical learning

#2.3.1 basic command
x=c(1,6,2)
y=c(1,4,3)

# length - check lenfth of variables
length(x)
length(y)
x+y

#ls():list all of variables / rm():delete any
ls()
rm(x,y)
ls()

#rm function - remove all objects at once
rm(list=ls())

# matrix function - creat a matrix of numbers
?matrix
x=matrix(data=c(1,2,3,4), nrow=2,ncol=2) #omit data, nrow, ncol

#matrix - byrow=TRUE option (populate matrix in order of row)
matrix(c(1,2,3,4), 2,2, byrow=TRUE)

#sqrt() function - returns squre root of each element of vector/matrix
sqrt(x)
x^2

#rnorm() function - genertes a vector of randam normal variables
x=rnorm(50) 
y=x+rnorm(50,mean=50,sd=0.1) #default (i.e.,mean 0, sd=1) can be changed with mean and sd
cor(x,y) 

#set.seed() function - takes an (arbitrary) integer argument
set.seed(1303) # used to perform calucations involving random quantities
rnorm(50)

rm(list=ls())
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

#2.3.2 Graphics
#plot() function - plot data in R
x=rnorm(100)
y=rnorm(100)

#====================================================================================================================
# Chapter 3 - 3.6. Lab: Linear Regression
#3.6.1 Libraries
library(MASS)
install.packages("ISLR")
library(ISLR)

#3.6.2 Simple Linear Regression
#MASS - Boston dataset
#medv:median house value, rm:average number of rooms, age:average age of houses, 
#lstat:percent of households with low socioeconomic status
require(MASS)
fix(Boston)
names(Boston)

lm.fit = lm(medv~lstat,Boston)
#define dataset used by attach function
attach(Boston) 
lm.fit = lm(medv~lstat) 

#regression result by lm (linear model) function
summary(lm.fit)
#names function: other information stored in lm.fit 
#coef: extracter functions of estimated coefficients
names(lm.fit) 
coef(lm.fit)

#confidential interval
confint(lm.fit)

#predict: produce confidence interval / prediction interval
predict(lm.fit, data.frame(lstat=(c(5,10,15))),
        interval = "confidence")
predict(lm.fit, data.frame(lstat=(c(5,10,15))),
        interval = "prediction")

#visualize
plot(lstat, medv)
##abline function: draw-line function
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="blue")
plot(lstat, medv, col="red")
plot(lstat, medv,pch=20)
plot(lstat, medv,pch="+")
plot(1:20,1:20, pch=1:20)

#Diagnostic plots:
#par function: splot display screen into separate panels 
par(mfrow=c(2,2))
plot(lm.fit)

#computation of residuals
plot(predict(lm.fit), residuals(lm.fit))
#studentized residual
plot(predict(lm.fit), rstudent(lm.fit))

#non-linearity leverage statistics
plot(hatvalues(lm.fit))
#which.max: identifies index of largest element of a vactor (obs with largest leverage statistics)
which.max(hatvalues(lm.fit))

#3.6.3 Multiple Linear Regression
#predictor:lstat and age
lm.fit2 = lm(medv~lstat+age, data=Boston)
summary(lm.fit2)

#predictor: 13 variables
lm.fit3 =lm(medv~.,data=Boston)
summary(lm.fit3)

#RSE: residual standard error
summary(lm.fit)$r.sq
summary(lm.fit)$sigma 

# vif: variance inflation function
install.packages("car")
library(car)
vif(lm.fit3)

#12variables case (exclude specific variable)
lm.fit4 = lm(medv~.-age,data=Boston)
summary(lm.fit4)
lm.fit5 = update(lm.fit3, ~.-age)

#3.6.4 Interaction terms
summary(lm(medv~lstat*age, data=Boston))

#3.6.5 Non-linear transformations of the Predictors
lm.fit2 = lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.fit2)

#anova function: quantify the quadratic fit
lm.fit = lm (medv~lstat)
anova(lm.fit, lm.fit2)
par(mfrow =c(2,2))
plot(lm.fit2)

#poly function: creation of polynomial within lm()
lm.fit5 <- lm(medv ~ poly (lstat,5))
summary(lm.fit5)
# log transformation
summary(lm(medv ~ log(rm), data=Boston))

#3.6.6 Qualitative Predictor
fix(Carseats)
names(Carseats)
lm.fit = lm(Sales ~.+Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# contrasts function: dummy variable
attach (Carseats)
contrasts(ShelveLoc)

#3.6.7 Writing Function
LoadLibraries()
LoadLibraries = function(){
  Library(ISLR)+
  Library(MASS)+
  print("The libraries have been loaded")
  }
LoadLibraries()

#====================================================================================================================
#Chap4: Classification coding
#4.6 Lab:ligstic Regression, LDA, QDA and KNN

## Dataset:Smarket data (ISLR libary)
## data: percentage returns for the S&P 500 stock index  over 1,250 days (2001 - 2005), 
## percentage returns for each of the five previous trading days: lag1 to lag5

library (ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

#cor function: produces matrix that contains all of the pariwise correlations among the predictors
cor(Smarket) #error - direction variable is qualitative
cor(Smarket[,-9]) 

# Correlation: the correlation between lag variables and toda's return is close to zero, substantial relation between Year and Volume
attach(Smarket)
plot(Volume)

#4.6.2 Logistic Regression
# Fit logstic regression model to predic Direction through lag1 to lag5 and Volume
glm.fit=glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial)
summary(glm.fit)

# model coefficient
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

# model prediction
## predict(): predict probability that the market will go up, given value of predictors
## "type=response" option: tell R to output probabilities of the form P(Y=1|X), 
## if no data set is supplied to the predict function,probabilities are computed fro the training data that was used to fit the model

## contrast(): indicates R has created a dummary variable with a 1 for Up
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)

## conversion of predicted probabilities into class labels [up or down]
## create a vector of class predictions based on whether the predicted probability of a market increase is greater than or less than 0
glm.pred=rep("Down", 1250) ## create a vector of 1,250 Down element
glm.pred[glm.probs>.5]="Up" ## transforms to up all of the elements where predicted probability exceeds 0.5

## table (): produce confusion matrix about how many obs were correctly classified
table(glm.pred, Direction)
(507+145)/1250
mean(glm.pred==Direction) ##the training error rate is 47.8%!

# Division of training set and held-out data
train=(Year<2005)

## Pick sub-matrix of stock market data set,correspoding to dates before 2005 
## ! symbol: reverse all of the elements of a Boolean vector
Smarket.2005=Smarket[!train,] 
dim(Smarket.2005)
Direction.2005=Direction[!train]

# Logistic regression for subset before 2005 / testing on 2005 sub-set
glm.fit=glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial, subset=train) #subset before 2005
glm.probs=predict(glm.fit, Smarket.2005,type="response") #subset on 2005
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005) #! means not equal to (test error rate)

# Refit logistic regiression model
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005, type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
predict(glm.fit, newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),type="response")

##4.6.3 Linear Discriminant Analysis (LDA)
install.packages("lda")
library(lda)
library(MASS)
library(ISLR) #Smarket dataset

# Fit lda model
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit

# predict function: three elements
## (1) Class: DA's prediction about market movement,
## (2) Posterior: matrix whose kth column includes the posterior probability
## (3) x: liniear discriminants
lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)

# LDA and logistic regression comparison (almost same results)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<0.5)
sum(lda.pred$posterior[,1]>0.9)

## 4.6.4: Quadratic Discriminant Analysis (QDA)
# Fit QDA model to Smarket data
qda.fit=qda(Direction~Lag1+Lag2, data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005) # QDA model fits better than LDA

## 4.6.5 K-Nearest Neighbors (KNN)

# Fuction overview
## knn function (): part of Class library, and forms prediction using a single command, requiring four items
## (1) a matrix containing predictors associated with training data (labeled "train.X") 
## (2) a matrix containing predictors associated with the data to make predictons (labeled "test.X)
## (3) a vector containing the class labes for the traiinng observations (labeled "train.Directon")
## (4) a value for K, the number of nearest neighbors to be used by the classfier

library(class)
library(ISLR)
train.X=cbind(Smarket$Lag1,Smarket$Lag2)[train,]
test.X=cbind(Smarket$Lag1,Smarket$Lag2)[!train,]
train.Direction=Smarket$Direction[train]

# application of KNN function (predict market movements for 2005)
set.seed(1)
knn.pred=knn(train.X, test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252

knn.pred=knn(train.X, test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

## 4.6.6 An Application to Caravan Insurance Data
require(ISLR)
dim(Caravan)
attach(Caravan)
summary(Purchase) # Purchase insurance data..

# data standardization (standard deviation, mean = zero)
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])


# KNN model fit
## data split into test set and training set
require(class)
test=1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred,test.Y)
9/(68+9)

knn.pred1=knn(train.X,test.X,train.Y,k=3)
table(knn.pred1,test.Y)
5/(5+21)

knn.pred2=knn(train.X,test.X,train.Y,k=5)
table(knn.pred2,test.Y)
4/(11+4)

# Logistic model fit
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.fit
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)

glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)

glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)
  
#======================================================================
http://www-bcf.usc.edu/~gareth/ISL/code.html
# Chapter5:Resampling methods (Cross-Validation and Bootstrap)

# 5.3.1 The validation set approach
## dataset: Auto

## set.seed(): set seed for random number generation
## sample (): split the set of obs (e.g., 196 from 392 obs)

library(ISLR)
set.seed(1)
train=sample(392,196)

#Linear Regression
lm.fit=lm(mpg~horsepower,data=Auto, subset=train) #subset - only traiing set

# Predict (): 
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2) #estimated MSE

## poly(): estimate test error for polynomial and cubic reg
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# different training set -> different results..
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
coef(lm.fit)


## 5.3.2 Leave-One-Out Cross-Validation (LOOCV)
#LOCCV - glm() and cv.glm()
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,subset=train)
coef(lm.fit)

## cv.glm (): part of "boot" library / procudes a list with several components
install.packages("boot")
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

## complex polynomial fits
## for (): loop/automate the process
cv.error=rep(0,5)
for (i in 1:5)
  {
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[i]
}
cv.error

glm.fit3=glm(mpg~poly(horsepower,3),data=Auto)
cv.error3 = cv.glm(Auto,glm.fit3)$delta
cv.error3

## a sharp drop in estimated test MSE between lineear and quadratic fits
## no clear improvement from using higher-order polynomials

# 5.3.3 K-Fold Cross-Validation

## cv.glm(): can also be used for k-fold CV (k=10 on Auto dataset)
## set a random seed / initialize vector to store CV erros corresponding 
set.seed (17)
cv.error.10=rep(0,10)
for (i in 1:10)
{
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

## K-Fold CV is quicker than LOOCV
## the above results show little evidence that using cubic or higher-order polynomial terms lead to lower test error than simply using a quadratic fit

## two numbers associated with delta are same as LOOCV
## (1) standard k-fold CV estimate, (2) bias-corrected version 


# 5.3.4 The Bootstrap

# Estimate accuracy of statistic of Interst (dataset = Portfolio (ISLR))
## two steps: 
#(1) create a function that computes statistic of interest 
#(2) use boot() function to perform the bootstrap by repeatedly sampling obs from data set with relacement

# Create alpha.fn ()
library(ISLR)
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

## alpha.fn: estimate ƒ¿ using all 100 obs
alpha.fn(Portfolio,1:100)

## sample (): randomly select 100 obs from range 1 to 100 with replacement
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

## Bootstrap: perform above command mutiple times / recording corresponding estimates, compute resulting standard deviation
## boot (): automate the approach (R = 1000 boostrap estimates)
boot(Portfolio,alpha.fn, R=1000)

# Estimate accuracy of a linear regresssion model
## boostrap method: assess variability of coefficient estimates/predictons

## create boot.fin(): returns intercept and slope estimate for linear model
boot.fn=function(data,index)+
return(coef(lm(mpg~horsepower, data=data,subset=index)))
dim(Auto)
boot.fin(Auto,1:392)

## Bootstrap estimate boot():compute standard erros of 1,000 bootstrap estimates
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coeff

# compute bootstrap standard error estimates standard linear regression estimates from fitting the quadratic model to the data
#This model has better fit to data, better correspondence between bootstrap estimates and standard estimates of SE(ƒÀ)

boot.fn=function(data,index)+
  coef(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

# Chap6. Linear Model Selection an Regularization
# 6.5 Lab1: Subset selection methods
# 6.5.1 Best subset selection

## dataset: Hitters data to predict baseball player's Salary from previous year's performance
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)

## dataset cleaning - exclusing of missing salary data

Hitters<-Hitters[!is.na(Hitters$Salary),]
fix(Hitters) #or 

sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

## regsubsets(): performs best sub-set selection / best 8 models
## identify best model conmtaining a give number of predictors (based on RSS)

install.packages("leaps")
library(leaps)
regfit.full=regsubsets(Salary~.,data=Hitters) 
summary(regfit.full)

## nvmax():return as many variables are desired
regfit.full=regsubsets(Salary~.,data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)

names(reg.summary)
reg.summary$rsq

## type1 option: connect plotted points with lines
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",tye="l")
plot(reg.summary$adjr2, xlab="Number of Variables",ylab="Adjusted RSq",type="l")

## points(): it puts points on a plot
## which.max():identify the location of max point of a vector

which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red",cex=0,pch=20)

# plot Cp and BIC instatistics using which.min(): 
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp",
     type='l')
which.min(reg.summary$cp)
points(10, reg.summary$cp[10],col="red",cex=2, pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC",
     type="l")
points(6, reg.summary$bic[6],col="red",cex=2,pch=20)

## regsubsets(); built-in plot command used to display selected variables 
## for best model with a given number of predictores ranked accordingg to BIC, Cp, R2 or AIC

plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

coef(regfit.full,6)

# 6.5.2 Foward and Backward Stepwise selection
## regsubsets(): foward/backward stepwise selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,
                      method="forward")
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,
                      method="backward")
summary(regfit.bwd)

## forward stepwise selection
## the best one variable model contains only CrBI and the best two-variable model additionally includes Hits
## best one-varabile through six-variables models are identified for best subset and foward selection
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# 6.5.3 Choosing Among Models, using the validation set approach and cross-validation
## how to choose among a set of models of different sizes using validation set/ cv aproachs

## step1: splot obs into training set and test set (random vector - train, elements==true)
set.seed(1)
train==sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)

## 









