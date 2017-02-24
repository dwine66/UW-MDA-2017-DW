# UW Data Science 350
# Winter 2017
# Dave Wine 8430191
# Assignment #7: Regression

## Libraries
require ('MASS')
require ('repr')
require ('ggplot2')
require ('dplyr')
require ('HistData')
require ('glmnet')
require ('gridextra')

## Read in datafile


## Functions

# File read function
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  auto.data <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
  
  numcols <- c('bore','stroke','horsepower','price','peak.rpm')
  auto.data[, numcols]<-lapply(auto.data[,numcols], as.numeric)
  
  factcols <- c('make','fuel.type','aspiration','num.of.doors','drive.wheels','engine.location','engine.type','num.of.cylinders','fuel.system','body.style')
  auto.data[, factcols]<-lapply(auto.data[,factcols], as.factor)
  
  auto.data[complete.cases(auto.data),]
}

####
# Main Code
####

# Read data in
auto.data <- read.auto()

# View dataset and summary statistics
str(auto.data)
summary(auto.data)

## Clean up dataset
## remove first two columns

auto.data <- auto.data[,-(1:2)]


## Transform price to log(price) based on previous work
auto.data$price <- log(auto.data$price)
##
##
## Check on factors
mod.mat <- model.matrix(price ~ ., data = auto.data)
mod.mat[1:10, ]

## Simple linear model
lm.price <- lm(price ~ ., data = auto.data)
summary(lm.price)
plot(lm.price)

### Stepwise Regression
lm.step <- stepAIC(lm.price, direction = 'both')
lm.step$anova # ANOVA of the result 
summary(lm.step) # Summary of the best model
plot(lm.step)


## Interaction Terms
lm.interaction = lm(childHeight ~ mother*father, data = males.ext)
summary(lm.interaction)
plot(lm.interaction)


## SVD
M = as.matrix(males.ext[, c('mother', 'father', 'mother.sqr', 'father.sqr')])
head(M)
MTM = t(M) %*% M
MTM

cat('Compute and print the inverse singular value matrix')
d.trim = rep(0, 4)
d.trim[1:2] =1/ mSVD$d[1:2]
mD = diag(d.trim)
mD
cat('Compute and print the pseudo inverse')
mInv = mSVD$v %*% mD %*% t(mSVD$u)
mInv
cat('Compute and print the dimensions of the matrix MTMTM')
MTMTM = mInv %*% t(M)
dim(MTMTM)



males.ext$score = M %*% b + mean(males.ext$childHeight)
males.ext$resids = males.ext$score - males.ext$childHeight

require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)

plot.svd.reg <- function(df, k = 4){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(df) + 
            geom_point(aes(score, resids), size = 2) + 
            stat_smooth(aes(score, resids)) +
            ggtitle('Residuals vs. fitted values')
 
  p2 <- ggplot(df, aes(resids)) +
           geom_histogram(aes(y = ..density..)) +
           geom_density(color = 'red', fill = 'red', alpha = 0.2) +
           ggtitle('Histogram of residuals')

  qqnorm(df$resids)
    
  grid.arrange(p1, p2, ncol = 2)
    
  df$std.resids = sqrt((df$resids - mean(df$resids))^2)  
    
  p3 = ggplot(df) + 
            geom_point(aes(score, std.resids), size = 2) + 
            stat_smooth(aes(score, std.resids)) +
            ggtitle('Standardized residuals vs. fitted values')
  print(p3) 
    
  n = nrow(df)
  Ybar = mean(df$childHeight)
  SST <- sum((df$childHeight - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))

  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}

plot.svd.reg(males.ext)
## Elastic Net Regression


cat('Compute and print the inverse singular value matrix')
lambda = 0.1
d.trim = 1/ (mSVD$d + lambda)
mD = diag(d.trim)
mD
cat('Compute and print the pseudo inverse')
mInv = mSVD$v %*% mD %*% t(mSVD$u)
mInv
MTMTM = mInv %*% t(M)

males.ext$score = M %*% b2 + mean(males.ext$childHeight)
males.ext$resids = males.ext$score - males.ext$childHeight

plot.svd.reg(males.ext)

require(glmnet)
b = as.matrix(males.ext$childHeight)
mod.ridge = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 0.0)
plot(mod.ridge, xvar = 'lambda', label = TRUE)
plot(mod.ridge, xvar = 'dev', label = TRUE)

mod.lasso = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 1.0)
plot(mod.lasso, xvar = 'lambda', label = TRUE)
plot(mod.lasso, xvar = 'dev', label = TRUE)

mod.ridge.lasso = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 0.5)
plot(mod.ridge.lasso, xvar = 'lambda', label = TRUE)
plot(mod.ridge.lasso, xvar = 'dev', label = TRUE)


males.ext$score = predict(mod.ridge.lasso, newx = M)[, 15]
males.ext$resids = males.ext$score - males.ext$childHeight

plot.svd.reg(males.ext)


Galton.scaled = GaltonFamilies[, c('mother', 'father', 'childHeight', 'gender')]
Galton.scaled = mutate(Galton.scaled, 
                       mother.sqr = mother^2, father.sqr = father^2)
Galton.scaled[, c('mother', 'father', 'mother.sqr', 'father.sqr')] = 
        lapply(Galton.scaled[, c('mother', 'father', 'mother.sqr', 'father.sqr')], 
               scale)
str(Galton.scaled)

mod.mat = model.matrix(childHeight ~ ., data = Galton.scaled)
mod.mat[1:10, ]

typeof(mod.mat[, 4])

b.mat = as.matrix(Galton.scaled$childHeight)
mod.ridge.gender = glmnet(mod.mat, b.mat, family = 'gaussian', 
                          nlambda = 20, alpha = 0.5)

Galton.scaled$score = predict(mod.ridge.gender, newx = mod.mat)[, 15]
Galton.scaled$resids = Galton.scaled$score - Galton.scaled$childHeight

plot.svd.reg(Galton.scaled)



