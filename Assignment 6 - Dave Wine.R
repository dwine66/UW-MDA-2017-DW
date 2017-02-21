# UW Data Science 350
# Winter 2017
# Dave Wine 8430191
# Assignment #6: Bayes
require ('LearnBayes')
require ('repr')
require ('ggplot2')
require ('dplyr')
# Probability of texting.
# You are asked to compute the probability that the driver of a car is texting at a specific intersection.
# Nationally the cumulative probability that a driver is texting is:
#  > P = 0.5, at x = 0.1
#  > P = 0.75 at x = 0.3
#- You observe cars at a location three times and note the number of texting drivers:
#  2 texting out of 20 drivers
#  4 texting out of 20 drivers
#  1 texting out of 20 drivers
# > Given these data
#- Compute the Beta prior, and report the coefficients 	
#- Plot the prior, likelihood and posterior three times as you update your belief based on collecting more data
#- Simulate the final posterior distribution and do the following:
#  > Plot the posterior with the 90% HDI shown
#> Report the upper and lower limits of the 90% HDI
#> Of the next hundred drivers what are the number of texting drivers in the 90% HDI?
#> Are the drivers in this area better or worse that the national figures indicate?

beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=0.3))
beta.par ## The parameters of my Beta distribution

## Functions
beta.plot <- function(dist,success,failure){
  dist + c(success, failure)
  options(repr.plot.width=6, repr.plot.height=5)
  triplot(dist, c(success,failure))
  dist  
}

post.sim <- function (post, title){
  options(repr.plot.width=8, repr.plot.height=5)
  post.sample <- rbeta(10000, post[1], post[2])
  par(mfrow = c(1,2))
  quants = quantile(post.sample, c(0.05, 0.95))
  breaks = seq(min(post.sample), max(post.sample), length.out = 41)
  hist(post.sample, breaks = breaks, 
       main = paste('Distribution of ',title,'samples \n with 90% HDI'),
       xlab = 'Sample value',
       ylab = 'Density')
  abline(v = quants[1], lty = 3, col = 'red', lwd = 3)
  abline(v = quants[2], lty = 3, col = 'red', lwd = 3)
  qqnorm(post.sample)
  par(mfrow = c(1,1))
  quants
}

drivers.hund <- function(dist,n,title){
  s <- 0:n
  pred.probs <- pbetap(dist, n, s)
  plot(s, pred.probs, type="h", 
       main = paste('Probability distribution of the number of',title,'drivers texting at the intersection, ', as.character(n), 'trials'),
       xlab = 'Successes')
  discint(cbind(s, pred.probs), 0.90)
  
  # This creates a CDF from the PDF. It's ugly from a programming standpoint, I know.
  dist.cdf <-rep(0,100)
  for (i in seq(1,n,1)){
    for (j in seq(1,i,1)) {
      dist.cdf[i]<-dist.cdf[i]+pred.probs[j]
    }
  }
  
  plot(dist.cdf)
  
  list("HDI" = discint(cbind(s, pred.probs), 0.90),"CDF" = dist.cdf,"PDF"=pred.probs)
}

## Main Code
# Plot the Prior
beta.plot(beta.par,0,0)

# Plot the successive cumulative evidence
beta.plot(beta.par,2,18) # results for first 20
beta.plot(beta.par,6,34) # results after 40
beta.plot(beta.par,7,53) # results after 60

# Simulate from the posterior and 
## compute confidence intervals

# The posterior is just the number of successes relative to the total number of trials
beta.post.par <- beta.par + c(2+4+1,18+16+19)

post.sim(beta.post.par, "Local Drivers")
post.sim(beta.par,"National Average")

# 7 successes out of 60 observations
predplot(beta.post.par, 60, 7)

# Now look at 100 drivers, both locally and nationally
local.pred <- drivers.hund(beta.post.par,100,'Local')
natl.pred <- drivers.hund(beta.par,100,'National')

# Compare the CDFs
PDF <- data.frame(local.pred$PDF,natl.pred$PDF)
CDF <- data.frame(local.pred$CDF,natl.pred$CDF)

# I tried to use ggplot for this but I could not figure it out for this plot....
ggplot(CDF,aes(x=row.names(CDF),y=local.pred$CDF,y=natl.pred$CDF))+geom_bar()

# Plot PDFs
plot(local.pred$PDF,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,.2))
par(new=TRUE)
plot(natl.pred$PDF,type='l',col ="blue",xlab="",ylab="")
title("Comparison of Local and National Drivers",xlab="Number of Drivers",ylab="Probability")
legend("bottomright",lwd=c(2,2),col=c("red","blue"),legend=c("Local","National"))

#Plot CDFs
plot(local.pred$CDF,type="l",col="red",xlab="",ylab="")
par(new=TRUE)
plot(natl.pred$CDF,type='l',col ="blue",xaxt="n",yaxt="n",xlab="",ylab="")
title("Comparison of Local and National Drivers",xlab="Number of Drivers",ylab="Cumulative Probability")
legend("bottomright",lwd=c(2,2),col=c("red","blue"),legend=c("Local","National"))
