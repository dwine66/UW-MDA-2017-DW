# UW Data Science Winter 2017
# Dave Wine 8430191
# Assignment 4

# Import packages
require(ggplot2)
require(car)
require(plyr)
require(dplyr)

# File read function
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  auto.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
  factcols <- c('price','peak.rpm')
  auto.data[, factcols]<-lapply(auto.data[,factcols], as.numeric)
  auto.data[complete.cases(auto.data),]
}

# Read data in
auto.data = read.auto()

# View dataset and summary statistics
str(auto.data)
summary(auto.data)

# Normality Testing of Price and log(Price)
price.log <- log10(auto.data$price)

# Significance tests
# ANOVA
# Price Difference Graphs using ANOVA and Tukey
