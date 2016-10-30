require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket

# Plot the values as pairs against each other and look at the binary responses
# use the Direction variable as the response variable
# the only two variables for which there is a clear binary division of 0s and 1s
# are Today and Direction, which is expected as Direction is a derivative of Today
pairs(Smarket, col=Smarket$Direction)

# Logistic Regression
# we fit the data to a logistic regression model by using the glm function
# and specifying the family as binomial
# Direction is the response variable, predictors are the OHLC and Volume
# We note none of the coefficiants are significant, but that might just be
# because the variables are correlated - useful predictions can still be made
glm.fit<- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

# We can make predictions from the fitted model,we assign to glm.probs the prection of glm.fit
# this will make predictions on the training data that we use to fit the model
# it predicts whether the market is going to be up or down based on data
# it produces a vector of fitted probabilities
# we look at the first 5 and see that they are very close to 50% which
# is not very surprising
glm.probs <- predict(glm.fit, type="response")
glm.probs[1:5]

# we can turn those predictions into classifications by thresholding at 0.5
# we do this by using the ifelse command, which creates a vector of logicals
# element by element is classified into UP if glm.probs prediction>50%, else 
# DOWN if prediction<50%
glm.pred <- ifelse(glm.probs>0.5, "Up", "Down")
glm.pred[1:5]

# now we look at our performance
# this plots a confusion matrix of predictions against actual outcomes
# based on the Direction variable as a response variable
# we see that there are lots of elements on the off diagonals
# we can also get our mean classification performance, which we find is 0.5216
# so on the training data, we do slightly better than chance
attach(Smarket)
table(glm.pred, Direction)
mean(glm.pred==Direction)

# we now divide our data into training set and test set
# we get a logical vector with TRUE values for Year < 2005
train <- Year<2005

# refit the glm.fit model, but this time utilising the test subset (i.e. NOT train)
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family=binomial, subset = train)
glm.probs <- predict(glm.fit, newdata=Smarket[!train, ], type="response")

# when we come to predict, we're now predicting on the remaining data
# which is the years 2005 and greater or the test data
glm.pred = ifelse(glm.probs>0.5, "Up", "Down")

# create a new variable for the test data
# make a table based on the predictions for test Direction data
# calcuate the mean classification performance again, which is 0.48 (worse than Null Rate of 0.5)
Direction.2005 <- Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

# the mean of 0.48 might be a result of overfitting
# so we resort to using a smaller model - use just the variables
# Lag1 and Lag2, leave out the rest, then go through all steps again
# we now get a mean classification performance of 56% which is not bad
# using the smaller model seems to have done bettter here
glm.fit <- glm(Direction ~ Lag1+Lag2, data = Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fit, newdata = Smarket[!train, ], type="response")
glm.pred <- ifelse(glm.probs>0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

#True Positive Rate, also known as Sensitivity or Recall (about 58.2%)
106/(76+106)

# Check if statistical significance increased by getting a smaller model
# Nothing became significant but at least the prediction of performance
# appeared to have increased
summary(glm.fit)

# That's it for fitting LOGISTIC REGRESSION MODELS IN R







