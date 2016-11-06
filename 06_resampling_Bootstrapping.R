require(ISLR)
require(boot)

# THE BOOTSTRAP METHOD
# this enables us to get at the sampling problem where we otherwise couldnt
# to illustrate, the example used in section 5.2 of the book where we have a 
# particularly non-linear formula for picking an optimal combination of two investments
# we deploy a function for the formula to compute alpha
alpha = function(x,y) {
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

# now we have a Portfolio dataframe with potential investments X and Y in it
# we run alpha on X and Y in the dataframe
alpha(Portfolio$X, Portfolio$Y)

# alpha comes up as 0.5758 but what is the standard error/variability of alpha?
# this is where we use the bootstrap function, but first in order to do that we
# need to create a little wrapper that allows a bootstrap to work
# what alpha.fn does is to use 'with' to take the first argument of data frame and some command
# then executes the command, and then computes alpha
# index will say which observations get repeated and which don't
# notice the parameters passed to alpha in 'with' are the actual variables in the data frame
# which is ver handy
alpha.fn <- function(data, index) {
  with(data[index, ], alpha(X,Y))
}

# run the function just once using the original indexed data
# we get the same 0.5758 as before so the function works
alpha.fn(Portfolio, 1:100)

# now we'll run the bootstrap to find standard error of alpha
# bootstrap involves random sampling and if we want to get reproducible results
# it's good to set the random number seed; so here we set seed1
# and run alpha.fn one more time, taking a random sample instead of a set index
# this is the kind of thing bootsrap does for us, here we do it just once
set.seed(1)
alpha.fn(Portfolio, sample(1:100, 100, replace = TRUE))

# instead of sampling 100 times just once, we call the boot(strap) function
# to do 1000 bootstraps now
boot.out <- boot(Portfolio, alpha.fn, R=1000)

#that was fast! now print out the summary result and plot the distribution
# we now have the standard error of alpha! and a Gaussian distribution
boot.out
plot(boot.out)


# So there's the bootstrap, a very handy way of getting very good reliable 
# estimates of standard error for nasty statistics

