require(ISLR)
require(boot)

# get some info on the K-Fold Cross Validation package for GLMs
?cv.glm

# plot data from the Auto data set: mpg against horsepower
plot(mpg~horsepower, data = Auto)


# Leave One Out Cross Validation (LOOCV)
# first fit a glm model to the data (can be a non-linear model as well)
# if you don't give the glm function a family name, it will just fit a linear model
# run cv.glm on that fitted model
# the cv.glm function produces two numbers - the first is the raw leave-one-out
# or lieu CV result, and the second one is a bias-corrected version of it
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.glm(Auto, glm.fit)$delta #pretty slow, predicts by brute force unlike the formula on page 180


# Lets write our own function using faster formula (5.2 on page 180)
# takes fit as a parameter and uses the lm.influence function which is a 
# post-processor for lm fit. It extracts the 'h' element from that which
# gives us the Hii diagonal elements. we use h in the vector calculations
# that follow to prodce the same answer as derived by function above
loocv <- function(fit) {
  h = lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

# we run the formula functio and get one result - the raw LIEU cross validation result
loocv(glm.fit)

# we now use our function to polynomials of different degrees to our data
# set up a vector to collect the prediction error generated by each degree polynomial
# write for loop to run the CV model 5 times
# polynomials of degree 1 up to 5 will be fitted
cv.error <- rep(0,5) #rep is the repeat function
degree <- 1:5
for (d in degree)
{
  glm.fit <- glm(mpg~ poly(horsepower, d), data = Auto)
  cv.error[d] <- loocv(glm.fit)
}

# print the errors to the console and plot them against their poly degree
cv.error
plot(degree, cv.error, type = 'b')


## 10-fold Cross Validation
# with 10-fold CV you actually do less work
# just divide the data into 10 equal parts and train on 9/10 parts
# the other 1/10 becomes our test set, and we only need to fit the model
# 10 times, as opposed to (potentially large) n times
cv.error10 <- rep(0,5)
for (d in degree)
{
  glm.fit <- glm(mpg~poly(horsepower, d), data=Auto)
  cv.error10[d] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
# plot the result of fitting using 10-fold CV to higher degree polynomials
# in this case, K-fold CV and LOOCV pretty much tell us the same story
lines(degree, cv.error10, type = 'b', col = 'red')





