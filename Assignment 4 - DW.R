# UW Data Science Winter 2017
# Dave Wine 8430191
# Assignment 4

# Import packages
require(ggplot2)
require(car)
require(plyr)
require(dplyr)

# Functions

# File read function
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  auto.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
  
  numcols <- c('price','peak.rpm')
  auto.data[, numcols]<-lapply(auto.data[,numcols], as.numeric)
  
  factcols <- c('fuel.type','aspiration','drive.wheels','body.style')
  auto.data[, factcols]<-lapply(auto.data[,factcols], as.factor)

  auto.data[complete.cases(auto.data),]
}

# Histogram Plot Function
plot.t <- function(a, b, cols = c(a, b), nbins = 20){
  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  par(mfrow = c(2, 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols[1]), xlab = cols[1])
  abline(v = mean(a), lwd = 4, col = 'red')
  hist(b, breaks = breaks, main = paste('Histogram of', cols[2]), xlab = cols[2])
  abline(v = mean(b), lwd = 4, col = 'red')
  par(mfrow = c(1, 1))
}

####
# Main Code
####

# Read data in
auto.data = read.auto()

# View dataset and summary statistics
str(auto.data)
summary(auto.data)

####

# Normality Testing of Price and log(Price)
price <- auto.data$price
price.log <- log(price)

# Graphical test using qqplot:
par(mfrow = c(1, 2))
qqnorm(price, main ='Q-Q plot of Price',sub=shapiro.test(price),col="red");qqline(price)
qqnorm(price.log, main = 'Q-Q plot of Log(Price)',col="blue");qqline(price.log)
par(mfrow = c(1, 1))
# Neither looks particularly normal. log(Price) may be somewhat more normal.
# This makes some sense in that there are very few cars cheaper than some minimum number.

# Formal Test using Shapiro-Wilk
shapiro.test(price)
shapiro.test(price.log)

# Yes, log(price) is much closer to normal than price - although still not particularly normal.
# So use that instead.
auto.data$price <- log(auto.data$price)
####

# Significance tests

# This function takes two quantities, queries the dataset, and returns the t-test and plots
sigtest <- function(plotvar, condvar,a,b){
  
  P1 <- filter(auto.data,condvar==a)
  P2 <- filter(auto.data,condvar==b)

  plot.t(P1$plotvar, P2$plotvar)  
  t.test(P1$plotvar,P2$plotvar, alternative = "two.sided")
  
}

# v <- sigtest('price','fuel.type','gas','diesel')
# I tried to write a function for this but couldn't figure out how to pass
# column names into the dplyr filter function.  It passes the headers but not the data!

auto.gas=filter(auto.data, fuel.type =='gas')
auto.diesel=filter(auto.data, fuel.type =='diesel')

auto.std=filter(auto.data, aspiration =='std')
auto.turbo=filter(auto.data, aspiration =='turbo')

auto.fwd=filter(auto.data, drive.wheels =='fwd')
auto.rwd=filter(auto.data, drive.wheels =='rwd')

plot.t(auto.gas$price, auto.diesel$price)
plot.t(auto.std$price, auto.turbo$price)
plot.t(auto.fwd$price, auto.rwd$price)

## Two-tailed test
t.test(auto.gas$price,auto.diesel$price, alternative = "two.sided")
t.test(auto.std$price,auto.turbo$price, alternative = "two.sided")
t.test(auto.fwd$price,auto.rwd$price, alternative = "two.sided")

####
# ANOVA - Doors
# Using log10(price) as it is closer to normal

auto.2d=filter(auto.data, num.of.doors == 'two')
auto.4d=filter(auto.data, num.of.doors == 'four')

df <- data.frame('group'=c(rep('2d',length(auto.2d$price)),
                           rep('4d',length(auto.4d$price))),
                 'val' = c(auto.2d$price,auto.4d$price))

df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)
boxplot(df$val ~ df$group)

df_aov = aov(val ~ group, data = df)
summary(df_aov)
print(df_aov)

tukey_aov = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_aov
plot(tukey_aov)
####
# ANOVA - Body Type
count(auto.data,body.style)

auto.con=filter(auto.data, body.style == 'convertible')
auto.har=filter(auto.data, body.style == 'hardtop')
auto.hat=filter(auto.data, body.style == 'hatchback')
auto.sed=filter(auto.data, body.style == 'sedan')
auto.wag=filter(auto.data, body.style == 'wagon')

df <- data.frame('group'=c(rep('Conv.',length(auto.con$price)),
                           rep('Hard',length(auto.har$price)),
                           rep('Hat.',length(auto.hat$price)),
                           rep('Sedan',length(auto.sed$price)),
                           rep('Wagon',length(auto.wag$price))),
                 'val' = c(auto.con$price,auto.har$price,auto.hat$price,auto.sed$price,auto.wag$price))

df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)
boxplot(df$val ~ df$group)

df_aov = aov(val ~ group, data = df)
summary(df_aov)
print(df_aov)

tukey_aov = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_aov
plot(tukey_aov)
