# UW Data Science 350
# Winter 2017
# Dave Wine 8430191
# Assignment #6: Bayes
require ('LearnBayes')
require ('repr')

# Probability of texting.
# You are asked to compute the probability that the driver of a car is texting at a specific intersection.
# Nationally the cumulative probability that a driver is texting is:
#  > P = 0.5, at x = 0.1
#  > P = 0.75 at x = 0.3
#- You observe cars at a location three times and note the number of texting drivers:
#  2 texting out of 20 drivers
#  4 texting out of 20 drivers
# 1 texting out of 20 drivers
# > Given these data
#- Compute the Beta prior, and report the coefficients 	
#- Plot the prior, likelihood and posterior three times as you update your belief based on collecting more data
#- Simulate the final posterior distribution and do the following:
#  > Plot the posterior with the 90% HDI shown
#> Report the upper and lower limits of the 90% HDI
#> Of the next hundred drivers what are the number of texting drivers in the 90% HDI?
#> Are the drivers in this area better or worse that the national figures indicate?

beta.par <- beta.select(list(p=0.5, x=0.2), list(p=0.75, x=.28))
beta.par ## The parameters of my Beta distribution

options(repr.plot.width=6, repr.plot.height=5)
triplot(beta.par, c(0, 0))

beta.par + c(6, 4)

# Simulate from the posterior and 
## compute confidence intervals
options(repr.plot.width=8, repr.plot.height=5)
beta.post.par <- beta.par + c(25, 15)
post.sample <- rbeta(10000, beta.post.par[1], beta.post.par[2])
par(mfrow = c(1,2))
quants = quantile(post.sample, c(0.05, 0.95))
breaks = seq(min(post.sample), max(post.sample), length.out = 41)
hist(post.sample, breaks = breaks, 
     main = 'Distribution of samples \n with 90% HDI',
     xlab = 'Sample value',
     ylab = 'Density')
abline(v = quants[1], lty = 3, col = 'red', lwd = 3)
abline(v = quants[2], lty = 3, col = 'red', lwd = 3)
qqnorm(post.sample)
par(mfrow = c(1,1))
quants

predplot(beta.post.par, 25, 15)

n <- 60
s <- 0:n
pred.probs <- pbetap(beta.post.par, n, s)
plot(s, pred.probs, type="h", 
     main = paste('Probability distribution of successes in', as.character(n), 'trials'),
     xlab = 'Successes')
discint(cbind(s, pred.probs), 0.90)