require(ISLR)
require(MASS)

# Linear Discriminant Analysis
?lda

# we use the lda command to fit an LDA model to data
# Direction is the response variable we're trying to predict
# using the stock market dataframe we saw when doing logistic regression
# we'll use a subset with years less than 2005, because later on we are
# going to try and make predictions for the year 2005
lda.fit <- lda(Direction ~ Lag1+Lag2, data = Smarket, subset=Year<2005)

# this call of the object's name gives us a summary of the Linear Discriminant Analysis
# plot the LDA fit, which we find is normally distributed
lda.fit
plot(lda.fit)

# we now want to see how well our rule predicts on the year 2005
# we'll use the subset command to make a subset of the Smarket dataframe
# and this will become our test data, or the place where we want to make our predictions
# we run a prediction function and R returns a list; the most convenient way to
# use the data is in a dataframe, so we use the data,frame command to coercively create one
Smarket.2005 <- subset(Smarket, Year==2005)
lda.pred <- predict(lda.fit, Smarket.2005)
data.frame(lda.pred)[1:5, ]

# we create the confusion matrix for the thing we're really interested in, which
# is the class (as we are doing classification, we want to see LDA classifying data)
# so we do a table of pred$class - the class label comes from the LDA fit and is
# a predicted class label for the 2005 Direction data; we put this up against the
# the true value/outcome in the confusion matrix
# our mean classification rate is 0.56, which in this industry actually helps!
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

# this ends the session on LINEAR DISCRIMINANT ANALYSIS IN R







