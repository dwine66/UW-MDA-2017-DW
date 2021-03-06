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
  treemap(dataset,var,size,type="index",palette="Greens",lowerbound.cex.labels = 0 ,title=name,aspRatio=1.5)
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
  par(mfcol=c(1,1),mai=c(0,0,0.25,0),oma=c(0,0,0,0))
  sPDF <- joinCountryData2Map(dataset, joinCode = "NAME", nameJoinColumn = njoin,verbose=TRUE)
  mapCountryData( sPDF, nameColumnToPlot=nplot,catMethod = seq(0,maxval,by = maxval/10),
                  colourPalette = "diverging", mapTitle = pname,addLegend = TRUE)
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
# iterates through multiple posterior sets and plots
iterate.Bayes <- function(dataset,YS,YE,YW){
  
  YearStart <- YS
  YearEnd <- YE
  YearWin <- YW
  
  colnames(dataset)[2] <-"count"
  
  dataset.prior <- filter(dataset,Year<YearStart)
  
  # Was there a death in the US due to foreign terrorism that year (a=no, b=yes)
  # Define beta prior based on US incidents from 1975 to 1990
  beta.par <-c(sum(dataset.prior$count==0),sum(dataset.prior$count>0))
  beta.par ## The parameters of my Beta distribution
  
  # Plot the Prior
  beta.plot(beta.par,0,0)
  
  # Plot the successive cumulative evidence over 
  
  par(mfcol=c(3,2), mai=c(.3,.3,.3,.3))
  beta.plot(beta.par,0,0)
  nFc <-0
  nnFc <-0
  # prior
  # I know this code is ugly, but it worked...
  for (i in seq(YearStart,YearEnd,by=YearWin)){
    nFatYear <- subset(dataset,Year>=i & Year<(i+YearWin))$count
    nFatFlag <- length(nFatYear[nFatYear!=0])
    nFc <- nFc + nFatFlag
    nnFc <- nnFc + YearWin-nFatFlag
    #  cat(i," ",nFatYear,(YearWin-nFc),nnFc,nFc,'\n')
    beta.plot(beta.par,nnFc,nFc) # results for each window 
    
  }
  list(nFc,beta.par)
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
### Setup Data
###

## Read data in
# Set base working directory
wd <- "C:/Users/Dave/Google Drive/UW Data Science/2017 Q1 - MDA/Term Project"
setwd(wd)

# Refugee flows
ref.data <-read.datafile("UNHCR_refugee_flows.csv",skip=5)
ref2.data <-read.datafile("20170305 UNHCR Refugee Data_US.csv",skip=4)
ref3.data <-read.datafile("Reporters_Entry_Data.csv")

# Terrorist Nationality
tnat.data <-read.datafile("20170306 Nowrasteh Terrorist Country data.csv")
#tnat2.data <-read.datafile("AN_Terrorist_Nationality.csv")

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
gtd.year <-dplyr::summarise(group_by(gtd.data,Year))

# Clean up refugee data
colnames(ref.data)[1] <-"Destination"
colnames(ref.data)[2] <-"Origin"

colnames(ref2.data)[1] <-"Year"
colnames(ref2.data)[2] <-"Destination"
colnames(ref2.data)[3] <-"Origin"
colnames(ref2.data)[4] <-"Type"
colnames(ref2.data)[5] <-"Number"


# Chop off data before 1988 - no origin data
ref2.data <- filter(ref2.data,Year>1987)
ref2.data <- filter(ref2.data,Number!="*")
# Turn NA's to zero where appropriate
#ref.data <- gsub("*","NA",ref.data)
ref.data[is.na(ref.data)] <-0

# Add summary data where appropriate 
ref.data$Total <- rowSums(ref.data[,4:15]) #through 2012
ref3.data$Total <- rowSums(ref3.data[,1:9]) #through 2015

# Clean up Terrorist Nationality Data
colnames(tnat.data)[5] <-"Origin"
colnames(tnat.data)[4] <-"Visa.Type"
colnames(tnat.data)[3] <-"Fatalities"
colnames(tnat.data)[1] <-"Terrorists"

str(ref.data)
str(tnat.data)
str(crime.data)
str(gtd.data)

###
### Main Code
###

# Map WW and US events, etc.
gtd.Country <- gtd.data %>% dplyr::count(country_txt)
country.plot(gtd.Country,"country_txt","n",20000, "Terror Attacks by Country, 1970-2015")

gtd.Country.TT <- dplyr::arrange(gtd.Country,desc(n)) %>% slice(1:10)

ggplot(gtd.Country.TT, aes(x=reorder(country_txt,-n),y=n)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(aspect.ratio=2/3) +
  ggtitle('Countries with the most terrorist attacks (1970-2015)') + xlab("Country") +ylab("Number of Attacks")

tnat.Country <- tnat.data %>% dplyr::count(Origin)
country.plot(tnat.Country,"Origin","n",20, "Identified Foreign Terrorist Country of Origin, 1975-2015")

# Event plot by country and year
gtd.CountryYear <- gtd.sub %>% group_by(Year) %>% dplyr::count(country_txt)

par(mfcol=c(3,2),mai=c(0.1,0.1,0.25,0.1))

for(i in seq(1970,2016,10)){
  dfil <- filter(gtd.CountryYear,Year>=i,Year<=i+9) %>% group_by(country_txt) %>% dplyr::summarise(sum(n))
  maxval <- max(dfil[,2])
  sPDF <- joinCountryData2Map(dfil, joinCode = "NAME", nameJoinColumn = "country_txt",verbose=TRUE)
  mapCountryData(sPDF, nameColumnToPlot="sum(n)",catMethod = seq(0,maxval,by = maxval/10),
                 colourPalette = "diverging", mapTitle = paste("Worldwide Terrorist Attacks by Country, ",i," -",i+9))
}

# Plot by latitude and longitude
map.plot(gtd.data,"Worldwide Terrorist Events - 1970-2015")

par(mai=c(0,0,.25,0),oma=c(0.25,0.25,0.25,0.25))
newmap <- getMap(resolution = "low")
plot(newmap,main="Terrorist Attacks in the US",xlim=c(-150,-75),ylim=c(15,65))
points(attacks.US.unk$longitude, attacks.US.unk$latitude,col='grey',pch=.5)
points(attacks.US.int$longitude, attacks.US.int$latitude,col='red',pch=1)
points(attacks.US.dom$longitude, attacks.US.dom$latitude,col='blue',pch=1)

# add legend

# US attacks only
map.plot(attacks.US,"Foreign Terrorist Events in the US",c(-150,-75),c(15,65))

# All attacks in the US in the 1970's


attacks.US.1970s <- filter(gtd.data,country_txt=="United States",Year>=1970,Year<=1979) %>% group_by(attacktype1_txt) %>% dplyr::count(attacktype1_txt)

attacks.US.1970s.groups <- filter(gtd.data,country_txt=="United States",Year>=1970,Year<=1979) %>% group_by(gname) %>% dplyr::count(gname)
attacks.US.1980s.groups <- filter(gtd.data,country_txt=="United States",Year>=1980,Year<=1989) %>% group_by(gname) %>% dplyr::count(gname)
attacks.US.1990s.groups <- filter(gtd.data,country_txt=="United States",Year>=1990,Year<=1999) %>% group_by(gname) %>% dplyr::count(gname)
attacks.US.2000s.groups <- filter(gtd.data,country_txt=="United States",Year>=2000,Year<=2009) %>% group_by(gname) %>% dplyr::count(gname)

attacks.US.unk.groups <- filter(gtd.data,country_txt=="United States",INT_IDEO==-9) %>% group_by(gname) %>% dplyr::count(gname)
attacks.US.dom.groups <- filter(gtd.data,country_txt=="United States",INT_IDEO==0) %>% group_by(gname) %>% dplyr::count(gname)
attacks.US.int.groups <- filter(gtd.data,country_txt=="United States",INT_IDEO==1) %>% group_by(gname) %>% dplyr::count(gname)


# by year

attacks.WW.1970s <- filter(gtd.data,Year>=1970,Year<=1979) %>% group_by(country_txt) %>% dplyr::count(country_txt)

par(mfcol=c(1,1))
hist(gtd.data$Year,breaks=46,main="worldwide attacks by year")

## Subset most relevant data

# INT_IDEO is the closest thing to country data in the GTD.
gtd.sub <- select(gtd.data,Year,country,country_txt,longitude,latitude,attacktype1_txt,targtype1, +
                    targtype1_txt,success,nkill,nkillus,nwound,nwoundus,INT_IDEO)

# Attacks that caused at least one death
gtd.sub.fatal <- filter(gtd.sub,nkill!="NA")
gtd.sub.fatal <- filter(gtd.sub,nkill>0)

gtd.sub.fatal.US <- filter(gtd.sub,nkill!="NA")
gtd.sub.fatal.US <- filter(gtd.sub,nkill>0,country_txt=="United States")

# Attacks that killed more than 1000 people
gtd.data.scary <- filter(gtd.data,nkill>1000)

# Fractional fatalities (more terrorists than victims)
gtd.data.low <- filter(gtd.data,nkill<1,nkill>0)

# US murder data
murders.sub <- select(filter(crime.data,Year>1969),Year,Murder) 

## Filter US data
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

ggplot(gtd.data, aes(x=reorder(targtype1_txt,targtype1_txt,function(x) -length(x)))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Bar chart of Terrorist Target Type: Worldwide')

ggplot(attacks.US, aes(x=reorder(targtype1_txt,targtype1_txt,function(x) -length(x)))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Bar chart of Terrorist Target Type: US')

ggplot(gtd.data, aes(x=reorder(targtype1_txt,targtype1_txt,function(x) -length(x)))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Bar chart of Terrorist Target Type: Worldwide')

# WW Boxplot
ggplot(gtd.sub.fatal, aes(x = factor(attacktype1_txt), y = nkill)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Boxplot of WW Terrorism Fatalities by Attack Type') + scale_y_log10() +coord_flip()

# US Boxplot
ggplot(gtd.sub.fatal.US, aes(x = factor(attacktype1_txt), y = nkill)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Boxplot of US Terrorism Fatalities by Attack Type') + scale_y_log10()

# Plot treemap of attack methodologies worldwide
atype.ww <- gtd.sub %>% dplyr::count(attacktype1_txt) 
atype.ww <- atype.ww[order(atype.ww$n),]
tree.plot(atype.ww,"attacktype1_txt","n","Attack Types (WW)")








## Bayesian Analysis

# Bayesian #1: Analysis of attacks before and after 1991

# Filter US attacks by international origin (exclude domestic & unknown) and if it caused a fatality
# Was there a death in the US due to foreign terrorism that year (a=no, b=yes)
# Define beta prior based on US incidents from 1975 to 1990

tdeaths.US <- select(attacks.US.int,Year,nkillus)
tdeaths.US <- filter(tdeaths.US,nkillus>=0)
tdeaths.US <- tdeaths.US %>% group_by(Year) %>% dplyr::summarise(tfatal=sum(nkillus))
# Correct for the fact that some years are missing - no events at all!
tdeaths.US <- left_join(gtd.year,tdeaths.US, by="Year")
tdeaths.US$tfatal[is.na(tdeaths.US$tfatal)] <-0

totfal=iterate.Bayes(tdeaths.US,1991,2015,5)

beta.par <- totfal[[2]]
# Simulate from the posterior and 
## compute confidence intervals

# The posterior is just the number of successes relative to the total number of trials
beta.post.par <- beta.par + c((YearEnd-YearStart)+1,totfal[[1]])

post.sim.pre <- post.sim(beta.par,"Pre-1990")
post.sim.pre
post.sim.post <- post.sim(beta.post.par, "Post-1990")
post.sim.post

# 7 successes out of 60 observations
#predplot(beta.post.par, 25, 2)

# Now look at the next 100 years
post1990.pred <- distpred(beta.post.par,100,'Post-1990')
pre1990.pred <- distpred(beta.par,100,'Pre-1990')

# Compare the CDFs
PDF <- data.frame(post1990.pred$PDF,pre1990.pred$PDF)
CDF <- data.frame(post1990.pred$CDF,pre1900.pred$CDF)

# Plot PDFs and CDFs
dist.plot(post1990.pred$PDF,pre1990.pred$PDF,"PDF of Pre- and Post 1990","Post-1990","Pre-1990")
dist.plot(post1990.pred$CDF,pre1990.pred$CDF,"CDF of Pre- and Post 1990","Post-1990","Pre-1990")

## Bayesian #2: Events - A slightly different question: 
# In a given year, was there an attempted foreign terrorist event or not (a = no, b= yes)

#Event-based
tevents.US <- select(filter(attacks.US.int),Year,success)
tevents.US <- tevents.US %>% group_by(Year) %>% dplyr::summarise(count=sum(success))
tevents.US <- left_join(gtd.year,tevents.US, by="Year")
tevents.US$count[is.na(tevents.US$count)] <-0

totevn=iterate.Bayes(tevents.US,1991,2015,5)

beta.par <- totevn[[2]]

beta.post.par <- beta.par + c((YearEnd-YearStart)+1,totevn[[1]])

post.sim.pre <- post.sim(beta.par,"Pre-1990")
post.sim.pre
post.sim.post <- post.sim(beta.post.par, "Post-1990")
post.sim.post

# 7 successes out of 60 observations
# predplot(beta.post.par, 25, 2)

# Now look at the next 100 years
post1990.pred <- distpred(beta.post.par,100,'Post-1990')
pre1990.pred <- distpred(beta.par,100,'Pre-1990')

priors <- left_join(tdeaths.prior,murders.prior,by = "Year")
events.prior <-left_join(tevents.US,murders.prior,by = "Year")

# of Fatalities and # events by country of origin
tnat.fat.Country <- tnat.data %>% group_by(Origin) %>% dplyr::summarize(Totalf = sum(Fatalities)) %>% arrange(Origin)
tnat.evn.Country <- tnat.data %>% group_by(Origin) %>% dplyr::count(Date) %>% dplyr::count(Origin) %>% arrange(Origin)
tnat.ter.Country <- tnat.data %>% group_by(Origin) %>% dplyr::count(Terrorists) %>% dplyr::count(Origin) %>% arrange(Origin)
tnat.ref.Country <- tnat.data %>% group_by(Origin) %>% dplyr::count(wt = Visa.Type=="R")
tnat.asy.Country <- tnat.data %>% group_by(Origin) %>% dplyr::count(wt = Visa.Type=="A")

tnat.Country <- cbind(tnat.evn.Country,tnat.fat.Country$Totalf,tnat.ter.Country$nn,tnat.ref.Country$n,tnat.asy.Country$n)
colnames(tnat.Country)[3] <- "nf"
colnames(tnat.Country)[4] <- "nt"
colnames(tnat.Country)[5] <- "nr"
colnames(tnat.Country)[6] <- "na"

tnat.Country <- arrange(tnat.Country,desc(nf))

ggplot(tnat.Country,aes(x=Origin,y=nn,fill=nf))  +
  geom_bar(stat="identity") + ylim(0,13) +
  coord_flip()+ggtitle("Foreign Terrorist Events in the US by Country of Origin, 1975-2015") +
  ylab("Number of Events") +
  scale_fill_gradient(low="darkblue",high="darkred", name="Number of Fatalites") +
  geom_text(aes(label=nf), hjust=1.5,vjust=.3,  color="white", size=3)

## Bayesian #3: Refugee Correlation Study

# Plot refugees from those countries that entered the US
ref2.Totals <- ref2.data %>% group_by(Origin) %>% dplyr::summarize(totalr=sum(as.numeric(Number)))

country.plot(ref2.Totals,"Origin","totalr",max(ref2.Totals$totalr), "Refugee entry to the US, 1988-2015")


# Filter to refugees that entered the US
ref.us <-filter(ref2.data,Destination=="United States",Population.type!="Returned refugees")

#Only have refugee data



# Join terrorist nationality and visa data to this
ref.us.ter <-inner_join(ref.us,tnat.Country,by="Origin")
#ref.us.ter <- filter(ref.us.ter,Terrorists!=0)
asy.us.ter <-filter(ref.us.ter,Population.type=="Asylum seekers")
ref.us.ter <-filter(ref.us.ter,Population.type=="Refugees")

ggplot(ref.us.ter,aes(log10(Total), nn)) +
  geom_point(aes(color=factor(Population.type))) +
  coord_fixed(1/5) +
  ylim(0,15) +
  ggtitle("")

ggplot(ref.us.ter,aes(x=Origin,y=(Total),fill=nt))  +
  geom_bar(stat="identity")  +
  coord_flip()+ggtitle("Foreign Refugees in the US by Country of Origin, 1975-2015") +
  ylab("Number of Refugees") +
  scale_fill_gradient(low="darkblue",high="darkred",name="Number of Terrorists") + # scale_fill_discrete(name="test")
  geom_text(aes(label=nt), hjust=-1.5,vjust=.3,  color="black", size=3)


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
# Well this conversion only took about 45 minutes to figure out....
tnat.data$Year <- as.POSIXlt(as.Date(tnat.data$Date, format='%m/%d/%Y'))$year+1900

tnat.ter.Year <- tnat.data %>% group_by(Year) %>% dplyr::count(Terrorists) %>% dplyr::count(Origin)
tnat.ref.Year <- tnat.data %>% group_by(Year) %>% dplyr::count(wt = Visa.Type=="R")
tnat.asy.Year <- tnat.data %>% group_by(Year) %>% dplyr::count(wt = Visa.Type=="A")


tnat.Year <- cbind(tnat.ter.Year,tnat.ref.Year$n,tnat.asy.Year$n)
colnames(tnat.Year)[2] <- "nt"
colnames(tnat.Year)[3] <- "nr"
colnames(tnat.Year)[4] <- "na"

tnat.Year <- left_join(gtd.year,tnat.Year, by="Year")
tnat.Year[is.na(tnat.Year)] <-0
tnat.Bayes <- filter(tnat.Year, Year>1988)

ref.Bayes <- left_join(ref3.data,tnat.Bayes,by="Year")

ref.Bayes <- mutate(ref.Bayes,Per=Refugee/Total)
ref.Bayes <- mutate(ref.Bayes,Pet=nt/Total)
ref.Bayes <- mutate(ref.Bayes,Prt=nr/nt)
ref.Bayes <- mutate(ref.Bayes,Ptr=Prt*Pet/Per)
ref.Bayes$Ptr[is.nan(ref.Bayes$Ptr)] <-0

ggplot(ref.Bayes,aes(x=Year,y=(Ptr)))  +
  geom_point(stat="identity")  +
  ggtitle("Bayesian Estimate of Terrorism Risk from Refugees, 1989-2015") +
  ylab("Probability that a refugee is a terrorist") 

### Function Test Area




