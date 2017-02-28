### UW Data Science 350
## Winter 2017
## Dave Wine 8430191
## Assignment #7: Regression

### Libraries
require ('MASS')
require ('repr')
require ('ggplot2')
require ('dplyr')
require ('HistData')
require ('glmnet')
require ('gridextra')

### Functions

# File read function
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  auto.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
  
  numcols <- c('bore','stroke','horsepower','price','peak.rpm')
  auto.data[, numcols]<-lapply(auto.data[,numcols], as.numeric)
  
  factcols <- c('make','fuel.type','aspiration','num.of.doors','drive.wheels','engine.location','engine.type','num.of.cylinders','fuel.system','body.style')
  auto.data[, factcols]<-lapply(auto.data[,factcols], as.factor)
  
  auto.data[complete.cases(auto.data),]
}

# Plot function
plot.svd.reg <- function(df, k = 4,modelname){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(df) + 
    geom_point(aes(score, resids), size = 2) + 
    stat_smooth(aes(score, resids)) +
    ggtitle(paste('Residuals vs. fitted values: ',modelname))
  
  p2 <- ggplot(df, aes(resids)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(color = 'red', fill = 'red', alpha = 0.2) +
    ggtitle(paste('Histogram of residuals: ',modelname))
  
  qqnorm(df$resids,main=paste("Normal Q-Q Plot: ",modelname))
  
  grid.arrange(p1, p2, ncol = 2)
  
  df$std.resids = sqrt((df$resids - mean(df$resids))^2)  
  
  p3 = ggplot(df) + 
    geom_point(aes(score, std.resids), size = 2) + 
    stat_smooth(aes(score, std.resids)) +
    ggtitle(paste('Standardized residuals vs. fitted values: ',modelname))
  print(p3) 
  
  n = nrow(df)
  Ybar = mean(df$price)
  SST <- sum((df$price - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}

EN.plot <-function(var,title){
  plot(var, xvar = 'lambda', label = TRUE,main=title)
  plot(var, xvar = 'dev', label = TRUE,main=title)
}

###
# Main Code
###

# Read data in
auto.data <- read.auto()

# View dataset and summary statistics
str(auto.data)
summary(auto.data)

## Clean up dataset
# remove first two columns
auto.data <- auto.data[,-(1:2)]

# Transform price to log(price) based on previous work
auto.data$price <- log(auto.data$price)

# Check on factors
auto.mat <- model.matrix(price ~ ., data = auto.data)
auto.mat[1:10, ]

# Simple linear model against all variables
lm.price <- lm(price ~ ., data = auto.data)
summary(lm.price)
plot(lm.price)

### Stepwise Regression
lm.step <- stepAIC(lm.price, direction = 'both')
lm.step$anova # ANOVA of the result 
summary(lm.step) # Summary of the best model
plot(lm.step)

### SVD
M = as.matrix(auto.mat)
MTM = t(M) %*% M
MTM 

# Create mSVD
mSVD <- svd(MTM)
mSVD$d

plot(mSVD$d,log="y",main ="log(mSVD)")

# Note that everything below about the 10th element is 10^4 less than the largest one. So zero out
# everything from there on down.
cat('Compute and print the inverse singular value matrix')
thres <-13
d.trim = rep(0, length(mSVD$d))
d.trim[1:thres] =1/ mSVD$d[1:thres]
mD = diag(d.trim)
mD
cat('Compute and print the pseudo inverse')
mInv = mSVD$v %*% mD %*% t(mSVD$u)
mInv
cat('Compute and print the dimensions of the matrix MTMTM')
MTMTM = mInv %*% t(M)
dim(MTMTM)

# Find model coefficients
b <- MTMTM %*% (auto.data$price - mean(auto.data$price))

# Compute Residuals
auto.data$score = M %*% b + mean(auto.data$price)
auto.data$resids = auto.data$score - auto.data$price

#Plot summary stats
require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)
plot.svd.reg(auto.data, thres,"SVD")

## Ridge Regression
cat('Compute and print the inverse singular value matrix')
lambda = 0.1
d.trim = 1/ (mSVD$d + lambda)
mD = diag(d.trim)
mD
cat('Compute and print the pseudo inverse')
mInv = mSVD$v %*% mD %*% t(mSVD$u)
mInv
MTMTM = mInv %*% t(M)

b2 <- MTMTM %*% auto.data$price

auto.data$score = M %*% b2 + mean(auto.data$price)
auto.data$resids = auto.data$score - auto.data$price

plot.svd.reg(auto.data,thres,"Ridge")

## Elastic Net Regression

require(glmnet)
b = as.matrix(auto.data$price)

mod.ridge = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 0.0)
EN.plot(mod.ridge,paste("Ridge with alpha = 0.0"))

mod.lasso = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 1.0)
EN.plot(mod.lasso,paste("Lasso with alpha = 1.0"))

mod.ridge.lasso = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 0.5)
EN.plot(mod.ridge.lasso,paste("Elastic Net with alpha = 0.5"))

## Evaluate the model

auto.data$score = predict(mod.ridge.lasso, newx = M)[, 15]
auto.data$resids = auto.data$score - auto.data$price
plot.svd.reg(auto.data,thres,"Elastic Net")

