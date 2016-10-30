library(MASS)
install.packages('ISLR') #download library from CRAN
library(ISLR) #load new library

##SImple linear regression
names(Boston)#get the variables in the Boston dataframe
?Boston #get further info on what the dataframe is about

#first thing we do is plot some variables
#the tilder between medv and lstat tells R medv is a response variable
#so medv will be plotted on the vertical axis against lstat
#in other words this is a plot of a linear regression of medv on lstat
#plot shows that with more lower-status occupants comes lower median house values
plot(medv~lstat, Boston) 


#fit a SIMPLE LINEAR REGRESSION MODEL to the data
# lm for linear model goes first, saved into a var called fit1
# the tilder regression again, with medv as response var, lstat as single predictor
# unlike for plotting, specify the data frame if fitting data to a lm
# summary will give us an intercept, coefficient, residuals etc
fit1 <- lm(medv~lstat, data=Boston)
summary(fit1) 

# add a linear model function to the plot
abline(fit1, col="red")

# you can get full names of the components of the fit, if you wish
names(fit1)

# you can get confidence intervals for the coefficients of the fit
# in this case it gives the lower 2.5% and upper 97.5% CI for each parameter/coefficient
confint(fit1)

# predict three new values for lstat i.e. 5, 10, 15
# also ask for a confidence interval with the prediction
predict(fit1, data.frame(lstat=c(5,10,15)), interval = "confidence")


## fitting MULTIPLE LINEAR REGRESSION
# variables used were lstat and age
# summary shows age is a significant factor but not as much as lstat
# also note the R-squared
fit2 <- lm(medv~lstat+age, data=Boston)
summary(fit2)

# fitting even more predictors to our linear our model
# this time we fitted all the variables except medv (which is the response)
# note that age is now no longer significant, unlike when regressed with lstat alone
# that means there are other predictors correlated with age
# in the presence of those other predictors, age is no longer required
fit3 <- lm(medv~., Boston)
summary(fit3)

# plot the fit3 in a 2by2 layout on screen, so we view many predictors at a time
# note nonlinearity ib the Residuals vs Fitted model for example
# that would tell us our model is not quite explaining everything going on there
par(mfrow=c(2,2))#create layout
plot(fit3)

# update fit3 
# from fit3, there is nothing on the left of tilder so we're using the same response
# dot means whatever the model was in fit3 (i.e. lm)
# minus sign means remove a predictor from the model
# summary confirms those 2 vars are out, all others left the same
fit4 <- update(fit3, ~. -age-indus)
summary(fit4)

## Accomodating nonlinearity and interactions
# star, in this instance, commands interactions to be factored in
# summary shows a main effect for lstat and age, then below that
# summary also shows an interaction lstat:age
# note: main effect for age not so significant, interaction much more significant
fit5 <- lm(medv~lstat*age, Boston)
summary(fit5)

# we saw there was a nonlinear plot between medv and lstat
# so therefore we fit lstat by putting in a quadratic term
# here the code for the quad term is protected by an identity function, I
# if the 'I' had not been there, the code would have meant something different
# to put two commands on same line, use a semicolon for separation
fit6 <- lm(medv~lstat +I(lstat^2), Boston); summary(fit6)

# use the 'attach' command to be able to access the data in the R search path
# create a 1x1 grid again, then plot lstat against medv
# use the 'points' command to find points and create a quadratic fit line
# abline cannot be used for that purpose anymore
# 'pch' means plotting character, which can be dots, squares, diamonds etc
attach(Boston)
par(mfrow = c(1,1))
plot(medv~lstat)
points(lstat, fitted(fit6), col='red', pch=20)

# there is an easier way of fitting polynomials, using the 'poly' function
# we'll now fit medv as a polynomial of degree 4 in lstat
# it is observed that the blue plot of degree 4 is starting to get too wiggly
# it is chasing and starting to overfit the data a little
# plotting character 18 means squares
fit7 <- lm(medv~poly(lstat,4))
points(lstat, fitted(fit7), col='blue', pch=18)


## Working with qualitative predictors (and dummy variables)
# the 'fix' command throws a graphical data editor for you in R for external eyeballing
# find the Carseats dataset and call the fix function on it
?Carseats
fix(Carseats)
names(Carseats)
summary(Carseats)

# we'll fit a model for the Carseats data, with Sales as response variable
# we'll regress against all variables and also add in some interactions
# between Income&Advertising and also Age&Price
fit8 <- lm(Sales~. +Income:Advertising + Age:Price, Carseats)
summary(fit8)

# the 'contrasts' function shows you how R coded a variable when fitting 
# in this case, it made Bad the baseline with 0 against Good and 0 against medium
# for the variable ShelveLoc, a qualitative variable
contrasts(Carseats$ShelveLoc)

# you can do some calculations as functions in order to avoid repeating them
# here we write a function called regplot to fit a linear model to 2 variables
# after performing simple regression we want to plot the fitted model
# so first thing is to name the function and say func_name <- function(parameters)
# then we write a function as normal and call it later in our code
regplot <- function(x, y)
{
    fit=lm(y~x)
    plot(x,y)
    abline(fit, col='red')
}

# attach the Carseats dataframe to the current R search path
# call the regplot function to fit a linear model to the data
# plot the resulting fit
attach(Carseats)
regplot(Price, Sales)

# we can make the regplot funtion more easily
# we call on the useful dot dot dot parameter construct in R
# this allows us to tell R we might want to specify unknown parameters
# and we can add extra arguments that will be passed and used as normal
regplot <- function (x, y,...)
{
    fit <- lm(y~x)
    plot(x, y,...)
    abline(fit, col='red')
}
regplot(Price, Sales, xlab='Price', ylab='Sales', col='blue', pch=20)















