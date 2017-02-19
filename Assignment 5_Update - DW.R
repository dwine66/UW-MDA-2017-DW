# UW Data Science Winter 2017
# Dave Wine 8430191
# Assignment 5 - Resampling


# Import packages
require(ggplot2)
require(car)
require(plyr)
require(dplyr)
require(HistData)
require(resample)
require(simpleboot)
require(repr)

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
plot.t <- function(a, b, plotvar,a.name, b.name,cols = c(a.name,b.name), nbins = 20){

  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  par(mfrow = c(2, 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols[1]), xlab = plotvar)
  abline(v = mean(a), lwd = 4, col = 'red')
  hist(b, breaks = breaks, main = paste('Histogram of', cols[2]), xlab = plotvar)
  abline(v = mean(b), lwd = 4, col = 'red')
  par(mfrow = c(1, 1))
}

# ANOVA Plot Function
ANOVA.plot <- function (df){
df$group = factor(df$group) # Make sure your groups are a factor (for further analysis below)
boxplot(df$val ~ df$group)

df_aov = aov(val ~ group, data = df)
summary(df_aov)
print(df_aov)

tukey_aov = TukeyHSD(df_aov)  # Tukey's Range test:
plot(tukey_aov)
print(tukey_aov)
}

boot.graph <-function(a,b,var,a.type,b.type){
mean.boot.a <- one.boot(a, mean, R = 100000)
mean.boot.b <- one.boot(b, mean, R = 100000)
plot.t(mean.boot.a$t, mean.boot.b$t, var,a.type,b.type,nbins = 80)
t.test(mean.boot.a$t,mean.boot.b$t, alternative = "two.sided")
}

plot.hist <- function(a, maxs, mins, cols = 'difference of means', nbins = 80, p = 0.05) {
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols), xlab = cols)
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = 0, lwd = 4, col = 'blue')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}

plot.diff <- function(a, cols = 'difference of means', nbins = 80, p = 0.05){
  maxs = max(a)
  mins = min(a)
  plot.hist(a, maxs, mins, cols = cols[1])
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

###
# Significance tests
###

# I tried to write a function for this but couldn't get it to pass
# columns into the dplyr filter function.  It passes the headers but not the data!
# It doesn't work for subset() either!!! Grrrrrr.

# This function takes two quantities, queries the dataset, and returns the t-test and plots
sigtest <- function(db, plotvar, condvar,aa,bb){
  
  print(bb)
  P1 <- subset(db,condvar==aa)#,select=plotvar)
  #P2 <- data.matrix(select(filter(db.data,condvar==b),plotvar))
  P2 <- subset(db,condvar==bb)#,select=plotvar)

#  plot.t(P1, P2)  
#  t.test(P1,P2, alternative = "two.sided")
  
}

v <- sigtest(auto.data,'price','fuel.type','gas','diesel')

# So do it the ugly way...
auto.gas=data.matrix(select(filter(auto.data, fuel.type =='gas'),price))
auto.diesel=data.matrix(select(filter(auto.data, fuel.type =='diesel'),price))

auto.std=data.matrix(select(filter(auto.data, aspiration =='std'),price))
auto.turbo=data.matrix(select(filter(auto.data, aspiration =='turbo'),price))

auto.fwd=data.matrix(select(filter(auto.data, drive.wheels =='fwd'),price))
auto.rwd=data.matrix(select(filter(auto.data, drive.wheels =='rwd'),price))

plot.t(auto.gas, auto.diesel, 'log(price)','Fuel Type = Gas','Fuel Type = Diesel')

## Now plot Resampled fuel types:
options(repr.plot.width=6, repr.plot.height=4)
two.boot.mean = two.boot(auto.gas, auto.diesel, mean, R = 100000)
plot.diff(two.boot.mean$t)

#Old graph
boot.graph(auto.gas,auto.diesel,'log(price)','Resampled Gas','Resampled Diesel')

plot.t(auto.std, auto.turbo, 'log(price)','Aspiration = std','Aspiration = turbo')
plot.t(auto.fwd, auto.rwd, 'log(price)','Drive Wheels = fwd','Drive Wheels = rwd')

## Now plot Resampled aspiration:
options(repr.plot.width=6, repr.plot.height=4)
two.boot.mean = two.boot(auto.std, auto.turbo, mean, R = 100000)
plot.diff(two.boot.mean$t)

boot.graph(auto.std,auto.turbo,'log(price)','Resampled std','Resampled turbo')

## Two-tailed test
t.test(auto.gas,auto.diesel, alternative = "two.sided")
t.test(auto.std,auto.turbo, alternative = "two.sided")
t.test(auto.fwd,auto.rwd, alternative = "two.sided")

###
# ANOVA - Doors
###

numdoors.count <- data.frame(count(auto.data,num.of.doors))
# There are 2 entries with bad data - discard

numdoors.count <- subset (numdoors.count, num.of.doors!='?')
ggplot(numdoors.count,aes(x=num.of.doors, y=n))+geom_bar(stat='identity')
# Plenty of data in both

auto.2d=filter(auto.data, num.of.doors == 'two')
auto.4d=filter(auto.data, num.of.doors == 'four')

df.door <- data.frame('group'=c(rep('2d',length(auto.2d$price)),
                           rep('4d',length(auto.4d$price))),
                 'val' = c(auto.2d$price,auto.4d$price))

ANOVA.plot(df.door)

###
# ANOVA - Body Type
###

body.count <- data.frame(count(auto.data,body.style))
ggplot(body.count,aes(x=body.style, y=n))+geom_bar(stat='identity')
# Hmm, convertibles and hardtops don't have much data - discard from ANOVA

#auto.con=filter(auto.data, body.style == 'convertible')
#auto.har=filter(auto.data, body.style == 'hardtop')
auto.hat=filter(auto.data, body.style == 'hatchback')
auto.sed=filter(auto.data, body.style == 'sedan')
auto.wag=filter(auto.data, body.style == 'wagon')

df.body <- data.frame('group'=c(rep('Hat.',length(auto.hat$price)),
                           rep('Sedan',length(auto.sed$price)),
                           rep('Wagon',length(auto.wag$price))),
                 'val' = c(auto.hat$price,auto.sed$price,auto.wag$price))

ANOVA.plot(df.body)

## Bootstrap the difference of the means of each pair:

options(repr.plot.width=6, repr.plot.height=4)
two.boot.mean = two.boot(auto.sed$price,auto.hat$price, mean, R = 100000)
plot.diff(two.boot.mean$t)

options(repr.plot.width=6, repr.plot.height=4)
two.boot.mean = two.boot(auto.wag$price,auto.hat$price, mean, R = 100000)
plot.diff(two.boot.mean$t)

options(repr.plot.width=6, repr.plot.height=4)
two.boot.mean = two.boot(auto.wag$price,auto.sed$price, mean, R = 100000)
plot.diff(two.boot.mean$t)
