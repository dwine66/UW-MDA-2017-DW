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
require(rworldmap)
require(treemap)
require(repr)

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
# Predicted distribution of n more trials
drivers.hund <- function(dist,n,title){
  s <- 0:n
  pred.probs <- pbetap(dist, n, s)
  plot(s, pred.probs, type="h", 
       main = paste('Probability distribution of the number of','title',as.character(n), 'trials'),
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

# Map
mapplot <-function(dataset,title,xr=c(-180,180),yr=c(-90,90)){
  par(mai=c(0,0,.25,0))
  newmap <- getMap(resolution = "low")
  plot(newmap,main=title,xlim=xr,ylim=yr)
  points(dataset$longitude, dataset$latitude,col='red',pch=".")
  
  par(mfcol=c(3,2),mai=c(0,0,0.25,0))
  
  for(i in seq(1970,2016,10)){
    tevents.ww <- filter(dataset,Year>=i,Year<=i+9)
    newmap <- getMap(resolution = "low")
    plot(newmap,main=paste("Terrorist Events: ",i,"'s"),xlim=xr,ylim=yr)
    points(tevents.ww$longitude, tevents.ww$latitude,col='red',pch=".")
  }
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

# US Crime database
crime.data <- read.datafile("2015 US Crime Rates.csv")

# Foreign Terrorist Visa List
visa.data <- read.datafile("2016 Terrorism and Risk_visalist.txt")

# GTD database
setwd(paste(wd,"/GTD_0616dist",sep=""))
gtd.data <- read.datafile("globalterrorismdb_0616dist.csv")

##  Clean up database
# Add high-level 1993 data to gtd
gtd.1993 <- read.datafile("gtd1993_0616dist_DW.csv")
gtd.data <-rbind.fill(gtd.data,gtd.1993)

# Rename columns as needed
colnames(gtd.data)[2] <-"Year"
colnames(ref.data)[1] <-"Destination"
colnames(ref.data)[2] <-"Origin"
colnames(tnat.data)[1] <-"Origin"

# Turn NA's to zero where appropriate
#ref.data <- gsub("*","NA",ref.data)
ref.data[is.na(ref.data)] <-0


# Add summary data where appropriate 
ref.data$Total <- rowSums(ref.data[,4:15]) #through 2012

#factcols <- c('country_txt')
#gtd.sub[, factcols]<-lapply(gtd.data[,factcols], as.factor)

str(ref.data)
str(tnat.data)
str(crime.data)
str(gtd.data)

# Subset most relevant data
gtd.sub <- select(gtd.data,Year,country,country_txt,longitude,latitude,attacktype1_txt,targtype1, +
                    targtype1_txt,success,nkill,nkillus,nwound,nwoundus,INT_IDEO)

gtd.sub.fatal <- filter(gtd.sub,nkill!="NA")
gtd.sub.fatal <- filter(gtd.sub,nkill>0)
gtd.sub.fatal$nkill <- log10(gtd.sub.fatal$nkill)

gtd.data.scary <- filter(gtd.data,nkill>1000)
gtd.data.low <- filter(gtd.data,nkill<1)
gtd.data.low <- filter(gtd.data.low,nkill>0)
# US murder data
murders.sub <- select(filter(crime.data,Year>1969),Year,Murder) 
murders.prior <- filter(murders.sub,Year<1991)

## Visualize basic data
attacks.US<-filter(gtd.sub,country_txt=="United States")
# Domestic vs International
attacks.US.dom <-filter(gtd.sub,country_txt=="United States",INT_IDEO==0)
attacks.US.int <- filter(gtd.sub,country_txt=="United States",INT_IDEO==1)
attacks.US.unk <- filter(gtd.sub,country_txt=="United States",INT_IDEO==-9)


types.US <- attacks.US %>% dplyr::count(targtype1_txt) 
types.US <- types.US[order(types.US$n),]
treemap(types.US,"targtype1_txt","n")

atype.ww <- gtd.sub %>% dplyr::count(attacktype1_txt) 
atype.ww <- atype.ww[order(atype.ww$n),]

treemap(atype.ww,"attacktype1_txt","n",type="index",palette="Blues",lowerbound.cex.labels = 0,title="Attack type (Worldwide)")

hist(attacks.US.int$Year, breaks=46,main = "Foreign Terrorist attacks in the US")
hist(attacks.US.dom$Year, breaks=46,main = "Domestic Terrorist attacks in the US")

# Bar plot of attack types
par(mfcol=c(1,1))
barplot(types.US$n,names.arg=types.US$targtype1_txt,horiz=TRUE)


ggplot(attacks.US, aes(x=reorder(targtype1_txt,targtype1_txt,function(x) -length(x)))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Bar chart of Terrorist Attack Type in US')

ggplot(gtd.data, aes(x=reorder(targtype1_txt,targtype1_txt,function(x) -length(x)))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Bar chart of Terrorist Attack Type ww')

ggplot(gtd.sub.fatal, aes(x = factor(attacktype1_txt), y = nkill)) + geom_boxplot() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Boxplot of Terrorism Fatalities')


# Map stuff

mapplot(gtd.data,"Terrorist Events - Worldwide")

par(mai=c(0,0,.25,0))
newmap <- getMap(resolution = "low")
plot(newmap,main="Terrorist Attacks in the US",xlim=c(-150,-75),ylim=c(15,65))
points(attacks.US.int$longitude, attacks.US.int$latitude,col='red',pch=1)
points(attacks.US.dom$longitude, attacks.US.dom$latitude,col='blue',pch=1)
points(attacks.US.unk$longitude, attacks.US.unk$latitude,col='green',pch=1)

mapplot(attacks.US,"Terrorist Events in the US",c(-150,-75),c(15,65))

# by year
hist(gtd.data$Year,breaks=46,main="worldwide attacks by year")

## Bayesian Analysis

# Filter US attacks by international origin (exclude domestic) and if it caused a fatality

tdeaths.US <- select(attacks.US.int,Year,nkillus)
tdeaths.US <- filter(tdeaths.US,nkillus>=0)
tdeaths.US <- tdeaths.US %>% group_by(Year) %>% dplyr::summarise(tfatal=sum(nkillus))
tdeaths.prior <- filter(tdeaths.US,Year<1991)
tdeaths.a <- filter(tdeaths.US,Year>=1991,Year<=1996)
tdeaths.b <- filter(tdeaths.US,Year>=1997,Year<=2001)
tdeaths.c <- filter(tdeaths.US,Year>=2002,Year<=2006)
tdeaths.d <- filter(tdeaths.US,Year>=2007,Year<=2011)
tdeaths.e <- filter(tdeaths.US,Year>=2012,Year<=2015)

# Define beta prior based on US incidents from 1975 to 1990

#Event-based
tevents.US <- select(filter(attacks.US,Year<1991,success==1),Year,success)
tevents.US <- tevents.US %>% group_by(Year) %>% dplyr::summarise(count=sum(success))
priors <- left_join(tdeaths.prior,murders.prior,by = "Year")
events.prior <-left_join(tevents.US,murders.prior,by = "Year")

barplot(tnat.data$Murders,names=tnat.data$Country)

# Was there a death in the US due to terrorism that year (a=no, b=yes)
#beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=.3))
beta.par <-c(15,4)
beta.par ## The parameters of my Beta distribution


# Plot the Prior
beta.plot(beta.par,0,0)

# Plot the successive cumulative evidence
par(mfcol=c(2,3))
beta.plot(beta.par,5,1) # results for first 20
beta.plot(beta.par,9,1) # results after 40
beta.plot(beta.par,14,0) # results after 60
beta.plot(beta.par,19,0) # results after 60
beta.plot(beta.par,24,0) # results after 60

# Simulate from the posterior and 
## compute confidence intervals

# The posterior is just the number of successes relative to the total number of trials
beta.post.par <- beta.par + c(25,2)

post.sim.local <- post.sim(beta.post.par, "Post-1990")
post.sim.local
post.sim.natl <-post.sim(beta.par,"Pre-1990")
post.sim.natl

# 7 successes out of 60 observations
predplot(beta.post.par, 25, 2)

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

## Refugee Data
ref.us <-filter(ref.data,Destination=="United States",Population.type!="Returned refugees")
ref.us.ter <-inner_join(ref.us,tnat.data,by="Origin")
ref.us.ter <- filter(ref.us.ter,Terrorists!=0)
#asy.us.ter <-filter(ref.us.ter,Population.type=="Asylum seekers")
#ref.us.ter <-filter(ref.us.ter,Population.type=="Refugees")

ggplot(ref.us.ter,aes(log10(Total), Terrorists)) +
  geom_point(aes(color=factor(Population.type))) +
  coord_fixed(1/5)

ggplot(ref.us.ter,aes(log10(Total), Murders)) +
  geom_point(aes(color=factor(Population.type))) 

ter.prob <- mutate(ref.us.ter,Prob_Ter=Terrorists/Total)

# Was there a terrorist in the refugee population (a=no, b=yes)
#beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=.3))
ref.OK.Total=sum(ref.us.ter$Total)-sum(ref.us.ter$Terrorists)
ref.ter.Total=sum(ref.us.ter$Terrorists)
beta.ter=c(ref.OK.Total,ref.ter.Total)
beta.plot(beta.ter,0,0)

#natl.pred <- drivers.hund(beta.par,100000,'Known Countries')
post.sim.ter <-post.sim(beta.ter,"Known Countries, 2000-2012")
post.sim.ter

options(repr.plot.width = 8,repr.plot.height = 4)

ref.us.tot=colSums(ref.us[4:16])
ref.us.prior=sum(ref.us.tot[4:9])
ref.us.post=sum(ref.us.tot[10:13])

barplot(ref.us.tot)

attacks.US.int.ref <- filter(attacks.US.int,Year>1999,Year<2013)
at.US.tot <- attacks.US.int.ref %>% group_by(Year) %>% dplyr::count(Year)
at.US.prior <- filter(at.US.tot,Year<2006)
at.US.post <- filter(at.US.tot,Year>2005)

US.comb.prior <- cbind(at.US.prior,ref.us.prior[1:6])

# Was there a terrorist in the refugee population (a=no, b=yes)
#beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=.3))
beta.ter <- c(80,4723932-80)
beta.plot(beta.ter,0,0)

beta.post.ter <- beta.ter+c(21,ref.us.post)

post.sim.local <- post.sim(beta.post.ter, "Post-2005")
post.sim.local
post.sim.natl <-post.sim(beta.ter,"Pre-2005")
post.sim.natl

predplot(beta.post.ter, 21, ref.us.post)

# Now look at 100 drivers, both locally and nationally
local.pred <- drivers.hund(beta.post.ter,10000,'Local')
natl.pred <- drivers.hund(beta.ter,10000,'National')

# Compare the CDFs
PDF <- data.frame(local.pred$PDF,natl.pred$PDF)
CDF <- data.frame(local.pred$CDF,natl.pred$CDF)
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
