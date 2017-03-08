### UW Data Science 350
## Winter 2017
## Dave Wine 8430191
## Assignment #8: Time Series

### Libraries
require ('repr')
require ('ggplot2')
require ('dplyr')
require ('HistData')
require ('gridExtra')
require ('forecast')

### Functions

# File read function
read.datafile = function(file = 'Automobile price data _Raw_.csv',skip=0){
  datafile.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE,skip=skip)
  
  #  numcols <- c('bore','stroke','horsepower','price','peak.rpm')
  #  gtd.data[, numcols]<-lapply(auto.data[,numcols], as.numeric)
  
  #  factcols <- c('make','fuel.type','aspiration','num.of.doors','drive.wheels','engine.location','engine.type','num.of.cylinders','fuel.system','body.style')
  #  gtd.data[, factcols]<-lapply(auto.data[,factcols], as.factor)
  
  #  gtd.data[complete.cases(auto.data),]
}

# Plot
dist.ts = function(df, col = 'residual', bins = 40){
  par(mfrow = c(1,2))
  temp = as.vector(df)
  breaks = seq(min(temp), max(temp), length.out = (bins + 1))
  hist(temp, breaks = breaks, main = paste('Distribution of ', col), xlab = col)
  qqnorm(temp, main = paste('Normal Q-Q plot of ', col))
  par(mfrow = c(1,1))
}

# ACF plots
plot.acf <- function(df, col = 'remainder', is.df =TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
  par(mfrow = c(1,1))
}

## Decomposition of the time series into components
ts.decomp <- function(df, col = 'elec.ts', span = 0.5, Mult = TRUE, is.df = TRUE){
  # if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with lowess span = ', as.character(span)))
  fit$time.series
}

## Function for ARIMA model estimation
ts.model = function(ts, col = 'remainder', order = c(0,0,1)){
  mod = arima(ts, order = order, include.mean = FALSE)
  print(mod)
  mod
}


### Main Code
wd <- "C:/Users/Dave/Documents/GitHub/DataScience350/DataScience350/Lecture8/"
setwd(wd)

# Dairy data
dairy.data <-read.datafile("CADairyProduction.csv")

### Main Code

#Do ice cream

#STL decomposition
icrm <-ts(dairy.data[,4],start = dairy.data[1,1],freq=12)
icrm.decomp <- ts.decomp(icrm, Mult=TRUE, is.df=FALSE,span =0.5)

#Plot ACF
options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf(icrm.decomp[, 3], is.df = FALSE)

# ARIMA modeling
icrm.arima = ts.model(icrm.decomp[, 3], col = 'ARIMA model for ice cream', order = c(2,1,2))
plot.acf(icrm.arima$resid[-1], is.df = FALSE)

# Forecasting
fit.icrm = auto.arima(icrm, max.p=3, max.q=3,
                       max.P=2, max.Q=2, max.order=5, max.d=0, max.D=0,
                       start.p=0, start.q=0, start.P=0, start.Q=0)
summary(fit.icrm)

## Make the forecast for the next year
elect.forecast = forecast(fit.icrm, h=12)
summary(elect.forecast)
plot(elect.forecast)

# Plot residuals
confint <- elect.forecast$upper-elect.forecast$lower
plot(confint,main="Confidence Intervals of 2014 Ice Cream Forecast")

