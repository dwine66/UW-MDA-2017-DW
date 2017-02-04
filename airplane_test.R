# Test dataset
require(ggplot2)
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  ## Read the csv file
  auto.price <- read.csv(file, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  auto.price[complete.cases(auto.price), ]
}
auto.price = read.auto()

str(auto.price)
price.log <- log10(auto.price$price)


hist(auto.price$price,breaks=100,freq=FALSE,density=10,col="red",+
       border="black",main="test",xlab="Price",ylab="frequency")
     
hist(price.log,breaks=50,freq=TRUE,density=10,col="red",border="black",main="test",xlab="Price",ylab="frequency")
