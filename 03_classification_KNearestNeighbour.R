library(class)

# K Nearest Neighbours
# these are simple but very effective
# experts say they do the best classification about half the time
?knn

attach(Smarket)


# create a matrix of Lag1 and Lag2 variables
# we also make an indicator variable train with TRUE for years less than 2005
Xlag <- cbind(Lag1, Lag2)
train <- Year<2005

# now we're ready call the knn function
# we pass in our matrix Xlag, indexed by train as the training? data
# for the test data we give it Xlag NOT train i.e. 2005 and later
# the response variable is Direction again, indexed by train
# k=1 says we want one nearest neighbour classification; so what the algorith
# does is to go into the training set in the x space (feature space) and look
# for the observation that's closest to your test point in Euclidian distance
# then classify to its class
knn.pred <- knn(Xlag[train, ], Xlag[!train, ], Direction[train], k=1)

# create a confusion matrix based on our variable of interest
# mean classification performance turns out to be 0.5
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])

# knn in this case was useless, it did no better than flipping a coin
# what could we try next? maybe try multiple values of k?



