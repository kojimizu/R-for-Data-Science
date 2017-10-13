
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






