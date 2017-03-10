### UW Data Science Winter 2017
## Dave Wine 8430191
## Term Project - Refugees, Terrorism, and Airstrikes

### Import packages

require(car)
require(plyr)
require(dplyr)
require(LearnBayes)

# For plotting
require(ggplot2)
require(gridExtra)
require(treemap)
require(repr)

# For mapping
require(rworldmap)

### Functions
# Bayesian Beta Plot
beta.plot <- function(dist,success,failure){
  dist + c(success, failure)
  options(repr.plot.width=6, repr.plot.height=5)
  triplot(dist, c(success,failure),where="topleft")
  dist  
}

# Bayesian Posterior Simulation
post.sim <- function (post, title){
  options(repr.plot.width=8, repr.plot.height=5)
  post.sample <- rbeta(10000, post[1], post[2])
  par(mfrow = c(1,2),mar=c(5.1,4.1,2.1,2.1))
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
distpred <- function(dist,n,title){
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

# Distribution Plot
dist.plot <- function(data1,data2,Name,dname1,dname2){
  plot(data1,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="") #,ylim=c(0,.2))
  par(new=TRUE)
  plot(data2,type='l',col ="blue",xlab="",ylab="")
  title(Name,xlab=dname1,ylab=dname2)
  legend("topright",lwd=c(2,2),col=c("red","blue"),legend=c(dname1,dname2))
}

# Treemap Plot
tree.plot <- function(dataset,var,size,name){
  par(mfcol=c(1,1),mai=c(0,0,.25,0))
  treemap(dataset,var,size,type="index",palette="Reds",lowerbound.cex.labels = 0 ,title=name,aspRatio=1.25)
}

# Incident Plotting
map.plot <-function(dataset,title,xr=c(-180,180),yr=c(-90,90)){
  par(mfcol=c(1,1),mai=c(0,0,.25,0))
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
  par(mfcol=c(1,1),mai=c(0,0,0.25,0))
}

# Plotting by country
country.plot <- function(dataset,njoin,nplot,maxval,pname){
  par(mfcol=c(1,1),mai=c(0,0,0.25,0))
  sPDF <- joinCountryData2Map(dataset, joinCode = "NAME", nameJoinColumn = njoin,verbose=TRUE)
  mapCountryData( sPDF, nameColumnToPlot=nplot,catMethod = seq(0,maxval,by = maxval/10),
                  colourPalette = "diverging", mapTitle = pname)
}

country.multi <-function(dataset,njoin,nplot,pname){

  par(mfcol=c(3,2),mai=c(0,0,0.25,0))
  
  for(i in seq(1970,2016,10)){
    dfil <- filter(dataset,Year>=i,Year<=i+9) %>% group_by("country_txt") %>% dplyr::summarise(sum(n))
    dfil.plot <- dfil[,2]
    maxval <- max(dfil.plot)
    sPDF <- joinCountryData2Map(dfil, joinCode = "NAME", nameJoinColumn = njoin,verbose=TRUE)
    mapCountryData(sPDF, nameColumnToPlot=dfil.plot,catMethod = seq(0,maxval,by = maxval/10),
                   colourPalette = "diverging", mapTitle = pname)
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

###
### Main Code
###

## Read data in
# Set base working directory
wd <- "C:/Users/Dave/Google Drive/UW Data Science/2017 Q1 - MDA/Term Project"
setwd(wd)

# Refugee flows
ref.data <-read.datafile("UNHCR_refugee_flows.csv",skip=5)
ref2.data <-read.datafile("20170305 UNHCR Refugee Data_US.csv",skip=4)

# Terrorist Nationality
tnat.data <-read.datafile("20170306 Nowrasteh Terrorist Country data.csv")
tnat2.data <-read.datafile("AN_Terrorist_Nationality.csv")

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

colnames(ref2.data)[2] <-"Destination"

colnames(tnat.data)[5] <-"Origin"
colnames(tnat.data)[4] <-"Visa.Type"
colnames(tnat.data)[3] <-"Fatalities"

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

## Subset most relevant data
# INT_IDEO is the closest thing to country data in the GTD.
gtd.sub <- select(gtd.data,Year,country,country_txt,longitude,latitude,attacktype1_txt,targtype1, +
                    targtype1_txt,success,nkill,nkillus,nwound,nwoundus,INT_IDEO)

# Attacks that caused at least one death
gtd.sub.fatal <- filter(gtd.sub,nkill!="NA")
gtd.sub.fatal <- filter(gtd.sub,nkill>0)
gtd.sub.fatal$nkill <- log10(gtd.sub.fatal$nkill)

# Attacks that killed more than 1000 people
gtd.data.scary <- filter(gtd.data,nkill>1000)

# Fractional fatalities (more terrorists than victims)
gtd.data.low <- filter(gtd.data,nkill<1,nkill>0)

# US murder data
murders.sub <- select(filter(crime.data,Year>1969),Year,Murder) 
murders.prior <- filter(murders.sub,Year<1991)

## Define US data
attacks.US<-filter(gtd.sub,country_txt=="United States")

# Domestic vs International
attacks.US.dom <- filter(gtd.sub,country_txt=="United States",INT_IDEO==0)
attacks.US.int <- filter(gtd.sub,country_txt=="United States",INT_IDEO==1)
attacks.US.unk <- filter(gtd.sub,country_txt=="United States",INT_IDEO==-9)

# Attack types in the US
types.US <- attacks.US %>% dplyr::count(targtype1_txt) 
types.US <- types.US[order(types.US$n),]

types.ww <- gtd.sub %>% dplyr::count(targtype1_txt) 
types.ww <- types.ww[order(types.ww$n),]

## Plot treemap of primary targets
tree.plot(types.US,"targtype1_txt","n","Target Types (US)")
tree.plot(types.ww,"targtype1_txt","n","Target Types (WW)")

hist(attacks.US.int$Year, breaks=46,main = "Foreign Terrorist attacks in the US")
hist(attacks.US.dom$Year, breaks=46,main = "Domestic Terrorist attacks in the US")

# Target Types
ggplot(attacks.US, aes(x=reorder(targtype1_txt,targtype1_txt,function(x) -length(x)))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Bar chart of Terrorist Target Type: US')

ggplot(gtd.data, aes(x=reorder(targtype1_txt,targtype1_txt,function(x) -length(x)))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Bar chart of Terrorist Target Type: Worldwide')

ggplot(gtd.sub.fatal, aes(x = factor(attacktype1_txt), y = nkill)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Boxplot of Terrorism Fatalities')

# Plot treemap of attack methodologies worldwide
atype.ww <- gtd.sub %>% dplyr::count(attacktype1_txt) 
atype.ww <- atype.ww[order(atype.ww$n),]
tree.plot(atype.ww,"attacktype1_txt","n","Attack Types (WW)")

# Map stuff
gtd.Country <- gtd.data %>% dplyr::count(country_txt)
country.plot(gtd.Country,"country_txt","n",20000, "Terror Attacks by Country, 1970-2015")

tnat.Country <- tnat.data %>% dplyr::count(Origin)
country.plot(tnat.Country,"Origin","n",20, "Identified Foreign Terrorist Country of Origin, 1975-2015")

#event grid plot
gtd.CountryYear <- gtd.sub %>% group_by(Year) %>% dplyr::count(country_txt)
#country.multi(gtd.CountryYear,"country_txt",n,"Worldwide Terrorist Events")

par(mfcol=c(3,2),mai=c(0.1,0.1,0.25,0.1))

for(i in seq(1970,2016,10)){
  dfil <- filter(gtd.CountryYear,Year>=i,Year<=i+9) %>% group_by(country_txt) %>% dplyr::summarise(sum(n))
  #dfil.plot <- dfil[,2]
  maxval <- max(dfil[,2])
  sPDF <- joinCountryData2Map(dfil, joinCode = "NAME", nameJoinColumn = "country_txt",verbose=TRUE)
  mapCountryData(sPDF, nameColumnToPlot="sum(n)",catMethod = seq(0,maxval,by = maxval/10),
                 colourPalette = "diverging", mapTitle = paste("Worldwide Terrorist Attacks by Country, ",i," -",i+9))
}

# Plot by latitude and longitude
map.plot(gtd.data,"Worldwide Terrorist Events - 1970-2015")

par(mai=c(0,0,.25,0))
newmap <- getMap(resolution = "low")
plot(newmap,main="Terrorist Attacks in the US",xlim=c(-150,-75),ylim=c(15,65))
points(attacks.US.unk$longitude, attacks.US.unk$latitude,col='grey',pch=.5)
points(attacks.US.int$longitude, attacks.US.int$latitude,col='red',pch=1)
points(attacks.US.dom$longitude, attacks.US.dom$latitude,col='blue',pch=1)
# add legend

# US attacks only
map.plot(attacks.US,"Foreign Terrorist Events in the US",c(-150,-75),c(15,65))

# All attacks in the US in the 1970's

attacks.WW.1970s <- filter(gtd.data,Year>=1970,Year<=1979) %>% group_by(country_txt) %>% dplyr::count(country_txt)
attacks.US.1970s <- filter(gtd.data,country_txt=="United States",Year>=1970,Year<=1979) %>% group_by(attacktype1_txt) %>% dplyr::count(attacktype1_txt)

attacks.US.1970s.groups <- filter(gtd.data,country_txt=="United States",Year>=1970,Year<=1979) %>% group_by(gname) %>% dplyr::count(gname)
attacks.US.1980s.groups <- filter(gtd.data,country_txt=="United States",Year>=1980,Year<=1989) %>% group_by(gname) %>% dplyr::count(gname)
attacks.US.1990s.groups <- filter(gtd.data,country_txt=="United States",Year>=1990,Year<=1999) %>% group_by(gname) %>% dplyr::count(gname)
attacks.US.2000s.groups <- filter(gtd.data,country_txt=="United States",Year>=2000,Year<=2009) %>% group_by(gname) %>% dplyr::count(gname)

# by year
par(mfcol=c(1,1))
hist(gtd.data$Year,breaks=46,main="worldwide attacks by year")

## Bayesian Analysis

# Bayesian #1: Analysis of attacks before and after 1991

# Filter US attacks by international origin (exclude domestic & unknown) and if it caused a fatality
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

# of Fatalities
tnat.Country <- tnat.data %>% group_by(Origin)%>% dplyr::summarize(Totalf = sum(Fatalities))
barplot(tnat.Country$Totalf,names=tnat.Country$Origin)

# Was there a death in the US due to foreign terrorism that year (a=no, b=yes)
#beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=.3))
beta.par <-c(sum(tdeaths.prior$tfatal==0),sum(tdeaths.prior$tfatal>0))
beta.par ## The parameters of my Beta distribution

# Plot the Prior
beta.plot(beta.par,0,0)

# Plot the successive cumulative evidence
par(mfcol=c(3,2), mai=c(.3,.3,.3,.3))
beta.plot(beta.par,0,0) # prior
beta.plot(beta.par,5,1) # results for first 20
beta.plot(beta.par,9,1) # results after 40
beta.plot(beta.par,14,0) # results after 60
beta.plot(beta.par,19,0) # results after 60
beta.plot(beta.par,24,0) # results after 60

# Simulate from the posterior and 
## compute confidence intervals

# The posterior is just the number of successes relative to the total number of trials
beta.post.par <- beta.par + c(25,2)


post.sim.pre <- post.sim(beta.par,"Pre-1990")
post.sim.pre
post.sim.post <- post.sim(beta.post.par, "Post-1990")
post.sim.post

# 7 successes out of 60 observations
predplot(beta.post.par, 25, 2)

# Now look at the next 100 years
post1990.pred <- distpred(beta.post.par,100,'Post-1990')
pre1990.pred <- distpred(beta.par,100,'Pre-1990')

# Compare the CDFs
PDF <- data.frame(post1990.pred$PDF,pre1990.pred$PDF)
CDF <- data.frame(post1990.pred$CDF,pre1900.pred$CDF)

# Plot PDFs and CDFs
dist.plot(post1990.pred$PDF,pre1990.pred$PDF,"PDF of Pre- and Post 1990","Post-1990","Pre-1990")
dist.plot(post1990.pred$CDF,pre1990.pred$CDF,"PDF of Pre- and Post 1990","Post-1990","Pre-1990")

## Bayesian #2: Refugee Correlation Study

# Filter to refugees that entered the US
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

# Bayesian #3: Countries with known terrorists
ref.us.tot=colSums(ref.us[4:16])
ref.us.prior=sum(ref.us.tot[4:9])
ref.us.post=sum(ref.us.tot[10:13])

barplot(ref.us.tot)

attacks.US.int.ref <- filter(attacks.US.int,Year>1999,Year<2013)
at.US.tot <- attacks.US.int.ref %>% group_by(Year) %>% dplyr::count(Year)
at.US.prior <- filter(at.US.tot,Year<2006)
at.US.post <- filter(at.US.tot,Year>2005)

US.comb.prior <- cbind(at.US.prior,ref.us.prior[1:6])

# How many terrorists were in the refugee population (a=terrorists, b=non-terrorists)
#beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=.3))
beta.ter <- c(80,ref.us.prior-80)
beta.plot(beta.ter,0,0)

beta.post.ter <- beta.ter+c(21,ref.us.post)

post.sim.local <- post.sim(beta.post.ter, "Post-2005")
post.sim.local
post.sim.natl <-post.sim(beta.ter,"Pre-2005")
post.sim.natl

predplot(beta.post.ter, 21, ref.us.post)

# Now look at the next 10000 refugees
post2005.pred <- distpred(beta.post.ter,10000,'Post-2005')
pre2005.pred <- distpred(beta.ter,10000,'Pre-2005')
#  Was the pre data a good predictor?

# Compare the CDFs
ref2005.PDF <- data.frame(post2005.pred$PDF,pre2005.pred$PDF)
ref2005.CDF <- data.frame(post2005.pred$CDF,pre2005.pred$CDF)

# Plot PDFs
dist.plot(post2005.pred$PDF,pre2005.pred$PDF,"PDF of Pre- and Post 2005","Post-2005","Pre-2005")
dist.plot(post2005.pred$CDF,pre2005.pred$CDF,"CDF of Pre- and Post 2005","Post-2005","Pre-2005")


#Do a classic disease analysis based on these numbers



### Function Test Area







