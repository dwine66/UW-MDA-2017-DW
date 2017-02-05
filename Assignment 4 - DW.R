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
  
  factcols <- c('fuel.type','aspiration','drive.wheels')
  auto.data[, factcols]<-lapply(auto.data[,factcols], as.factor)

  auto.data[complete.cases(auto.data),]
}

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

# Read data in
auto.data = read.auto()

# View dataset and summary statistics
str(auto.data)
summary(auto.data)

####

# Normality Testing of Price and log(Price)
price <- auto.data$price
price.log <- log10(price)

# Graphical test using qqplot:
par(mfrow = c(1, 2))
qqnorm(price, main ='Q-Q plot of Price',sub=shapiro.test(price),col="red");qqline(price)
qqnorm(price.log, main = 'Q-Q plot of Log10(Price)',col="blue");qqline(price.log)
par(mfrow = c(1, 1))
# Neither looks particularly normal. log(Price) may be somewhat more normal.
# This makes some sense in that there are very few cars cheaper than some minimum number.

# Formal Test using Shapiro-Wilk
shapiro.test(price)
shapiro.test(price.log)

qqnorm(price.log, main = 'Q-Q plot of Log10(Price)');qqline(price.log)
# Yes, log(price) is much closer to normal than price - although still not particularly normal.

####

# Significance tests

# This function takes two quantities, queries the dataset, and returns the t-test and plots
sigtest <- function(dataset,a,b){
  q1=data
  
  
}
auto.gas=filter(auto.data, fuel.type =='gas')
auto.diesel=filter(auto.data, fuel.type =='diesel')

auto.std=filter(auto.data, aspiration =='std')
auto.turbo=filter(auto.data, aspiration =='turbo')

auto.4wd=filter(auto.data, drive.wheels =='4wd')
auto.fwd=filter(auto.data, drive.wheels =='fwd')
auto.rwd=filter(auto.data, drive.wheels =='rwd')

plot.t(auto.gas$price, auto.diesel$price)
plot.t(auto.std$price, auto.turbo$price)
plot.t(auto.4wd$price, auto.fwd$price)

## Two-tailed test
t.test(auto.gas$price,auto.diesel$price, alternative = "two.sided")
t.test(auto.std$price,auto.turbo$price, alternative = "two.sided")
t.test(auto.4wd$price,auto.rwd$price, alternative = "two.sided")

####
# ANOVA

auto.2d=filter(auto.data, num.of.doors == 'two')
auto.4d=filter(auto.data, num.of.doors == 'four')

df <- data.frame('group'=c(rep(1,length(auto.2d$price)),
                           rep(2,length(auto.4d$price))),
                 'val' = c(auto.2d$price,auto.4d$price))

df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)
boxplot(df$val ~ df$group)

df_aov = aov(val ~ group, data = df)
summary(df_aov)
print(df_aov)

tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova
####

# Price Difference Graphs using ANOVA and Tukey
