### UW Data Science Winter 2017
## Dave Wine 8430191
## Term Project - Refugees, Terrorism, and Airstrikes

# Perspectives
# 1. You are a US customs official with a long line of refugees.  You need to decide whether each is a potential
# based on their sex, country of origin, and religion.
# 2. You are a terrorist mastermind trying to recruit people who will not arouse US customs supervision.
# 3. You are the President of the United States trying to choose an entry policy.
# 4. You are a citizen of the United States that is voting for a President that is claiming extreme danger
# from foreign terrorists posing as refugees.
# 5. You are a citizen of one of those countries, which is currently subject to airstrikes by the United
# States.  How safe are you compared to the US citizen?
# 6. You are a policy wonk in the US government, trying to decide whether to add additional screening to 
# a refugee admission process.

### Import packages
require(ggplot2)
require(gridExtra)
require(car)
require(plyr)
require(dplyr)
require(LearnBayes)

### Functions

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
  
  list(quants,paste("Mean: ",mean(post.sample)),paste("Median: ",median(post.sample)))
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

# File read function
read.datafile = function(file = 'Automobile price data _Raw_.csv',skip=0){
  datafile.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE,skip=skip)
  
#  numcols <- c('bore','stroke','horsepower','price','peak.rpm')
#  gtd.data[, numcols]<-lapply(auto.data[,numcols], as.numeric)
  
#  factcols <- c('make','fuel.type','aspiration','num.of.doors','drive.wheels','engine.location','engine.type','num.of.cylinders','fuel.system','body.style')
#  gtd.data[, factcols]<-lapply(auto.data[,factcols], as.factor)
  
#  gtd.data[complete.cases(auto.data),]
}

### Main Code

## Read data in
# Set base working directory
wd <- "C:/Users/Dave/Google Drive/UW Data Science/2017 Q1 - MDA/Term Project"
setwd(wd)

# Refugee flows
ref.data <-read.datafile("UNHCR_refugee_flows.csv",skip=5)

# Terrorist Nationality
tnat.data <-read.datafile("AN_Terrorist_Nationality.csv")

# GTD database
setwd(paste(wd,"/GTD_0616dist",sep=""))
gtd.data <- read.datafile("globalterrorismdb_0616dist.csv")

#clean up database

# View dataset and summary statistics
str(ref.data)
str(tnat.data)
str(gtd.data)

# Subset most relevant data
gtd.sub <- select(gtd.data,iyear,country,country_txt,attacktype1,success,nkill,nkillus,nwound,nwoundus,INT_IDEO)

## Visualize basic data

factcols <- c('country_txt')
gtd.sub[, factcols]<-lapply(gtd.data[,factcols], as.factor)

hist(gtd.data$iyear,breaks=46)

# Filter US attacks by international origin (exclude domestic)
attacks.US <- filter(gtd.data,country_txt=="United States",INT_IDEO==1)
hist(attacks.US$iyear, breaks=46,main = "Foreign Terrorist attacks in the US")

# Look at US fatalities by year
deaths.us <- count(attacks.US,nkillus)
# Define beta distribution based on worldwide incidents from 1975 to 1990

prior <- filter(gtd.sub,iyear<1991)
count.success <- sum(prior$success)

barplot(tnat.data$Murders,names=tnat.data$Country)

### Assignment 6 


beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=0.3))
beta.par ## The parameters of my Beta distribution

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

post.sim.local <- post.sim(beta.post.par, "Local Drivers")
post.sim.local
post.sim.natl <-post.sim(beta.par,"National Average")
post.sim.natl

# 7 successes out of 60 observations
predplot(beta.post.par, 60, 7)

# Now look at 100 drivers, both locally and nationally
local.pred <- drivers.hund(beta.post.par,100,'Local')
natl.pred <- drivers.hund(beta.par,100,'National')

# Compare the CDFs
PDF <- data.frame(local.pred$PDF,natl.pred$PDF)
CDF <- data.frame(local.pred$CDF,natl.pred$CDF)

# I tried to use ggplot for this but I could not figure it out for this plot....
#ggplot(CDF,aes(x=row.names(CDF),y=local.pred$CDF,y=natl.pred$CDF))+geom_bar()

# Plot PDFs
plot(local.pred$PDF,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,.2))
par(new=TRUE)
plot(natl.pred$PDF,type='l',col ="blue",xlab="",ylab="")
title("Comparison of Local and National Drivers",xlab="Number of Drivers",ylab="Probability")
legend("topright",lwd=c(2,2),col=c("red","blue"),legend=c("Local","National"))

#Plot CDFs
plot(local.pred$CDF,type="l",col="red",xlab="",ylab="")
par(new=TRUE)
plot(natl.pred$CDF,type='l',col ="blue",xaxt="n",yaxt="n",xlab="",ylab="")
title("Comparison of Local and National Drivers",xlab="Number of Drivers",ylab="Cumulative Probability")
legend("bottomright",lwd=c(2,2),col=c("red","blue"),legend=c("Local","National"))

