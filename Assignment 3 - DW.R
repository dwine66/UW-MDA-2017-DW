# UW - DSC - MDA Winter 2017
# Assignment 3
# Dave Wine 8430191

# Add packages
require(ggplot2)
require(plyr)

# Variables
doors <- c(1,2,3)
prizes <- c('Goat','Goat','Goat')
states <- c('Closed','Closed','Closed')

# Define number of games
n <- 10

# Theoretical probabilities of winning a car
p.stay = 0.334
p.switch = 0.667

# Functions
###############

# Game Function

game.instance <- function(pc,n){
  #initialize counter
  print("Starting Game")
  
  result.car <- 0
  for (i in 1:n){

    prizes <- c('Goat','Goat','Goat')
    states <- c('Closed','Closed','Closed') 
 #   print(states)
    door.car <- doors.load()
    cat("Car is behind Door",door.car,"\n")
    prizes[door.car] <- "Car"

    door.guess <- player.guess()
    states[door.guess] <- 'Guessed'
    cat("Player guessed Door",door.guess,"\n")

    host.open <- host.reveal(door.car, door.guess)
    states[host.open] <- "Host Opened"
    cat("Host opens Door", host.open,"\n")
    
    player.choice <- player.choose(pc,door.guess,states)
    states[player.choice] <- "Chosen"
    cat("Player decides to",pc," and host opens door",player.choice,"\n")
  
    player.prize <- host.open(player.choice,door.car)
    # then add outcome to result set
      if (player.prize == 'Car') {result.car = result.car + 1
      }
    cat("Player Wins a",player.prize,"\n\n")

  }
  result.car
}
# Pick Another - recursively chooses another number that isn't the one passed to the function
pick.another <-function(not.this.one,all.possible) {
  x <- as.integer(runif(1,0,all.possible)+1 )
  if(x == not.this.one) pick.another(not.this.one,all.possible) else x
}

# Pick one door at random to hold the car
doors.load <- function(){
  x <- as.integer(runif(1,0,3))+1
}

# Contestant first guess
player.guess <- function(){
  x <- as.integer(runif(1,0,3))+1
}

# Host reveals door of his choice
host.reveal <-function(car,guess){
  # if the player's guess is correct, choose a goat at random
  if (guess == car) {
    hr <- pick.another(car,3) 
  }
  # otherwise choose the door that isn't the car or the door the player chose
  else {hr <- which(doors!=car & doors!=guess)
  }

}

# Contestant chooses from remaining doors
player.choose <- function(pc,dg,st){
  #doors.left=subset(doors,states[-"Opened"]) # Can only choose from doors that are not opened
print (st)
  if (pc == "Stay") {
    ppc <- dg
  }
  else {ppc <- which("Closed"==st) # Switch to the closed, unchosen door
  }
}

# Host reveals contestant door 
host.open <- function(pch,dc){
  if (pch == dc) {
    pp <- "Car" 
  }
  else {pp <- "Goat"
  }
}

# Binomial plot function (stolen proudly from Distributions.R!)
binom.plot <- function(data.df,num.runs,p){
  N = num.runs # parameter list
  #binom_samples = lapply(N, function(x) rbinom(nb, x, p))  # Compute list of random draws
  
  binom_samples =data.df  # Compute list of random draws
  
  binom_sample_means = lapply(binom_samples, mean)  # Compute list of sample means
  binom_means = N*p
  data.frame(BinomialMean = binom_means, SampleMean = unlist(binom_sample_means))
  
  binom_sample_vars = lapply(binom_samples, var) # Compute list of sample variance
  binom_vars = N*p*(1-p)
  data.frame(BinomialVariance = binom_vars, SampleVariance = unlist(binom_sample_vars))
  
  par(mfrow=c(1,3))
  invisible(lapply(binom_samples, function(x) hist(x))) # histograms of random draws
  par(mfrow=c(1,1))
  
  # Compare Normal Approximation to binomial
  par(mfrow=c(1,3))
  for (i in 1:3){
    hist(binom_samples[[i]], main=paste(N[i],'Experiments'), freq=FALSE)
    x_norm = seq(0,N[i], by = 0.025)
    y_norm = dnorm(x_norm, mean=binom_means[i], sd=sqrt(binom_vars[i]))
    lines(x_norm, y_norm)
  }
  par(mfrow=c(1,1))
}

###############

# Main Loop

# Run the same number of games with the contestant 'Staying' vs. 'Switching'.
# You could also randomize the last player choice and then sort out the wins from not, 
# but this is equivalent and easier.
# Output is number of cars won after n games in each case.

#car.stay <- game.instance("Stay",n)
#car.switch <- game.instance("Switch",n)

# m is the ensemble counter - that is, we will run m sets of n games
m <- 100

# Set up data frames to hold ensemble results
car.stay.df <-data.frame(low.st=rep(NA,m),med.st=rep(NA,m),hi.st=rep(NA,m))
car.switch.df <-data.frame(low.sw=rep(NA,m),med.sw=rep(NA,m),hi.sw=rep(NA,m))

for (i in 1:m){
n <- c(10, 100, 1000) # number of games in an ensemble

car.stay <- laply(n, function(n) game.instance("Stay",n))
car.switch <- laply(n, function(n) game.instance("Switch",n))

car.stay.df[i,] <- car.stay
car.switch.df[i,] <- car.switch

}

car.stay.binom <- binom.plot(car.stay.df,n,p.stay)
car.switch.binom <- binom.plot(car.switch.df,n,p.switch)

# Plot Results
# Basically, show as n goes to infinity, the probabilities converge to the theoretical predictions
#car.stay.ens.p <- laply(n, function(n) car.stay/n)
#car.switch.ens.p <- car.switch.ens./n

car.stay.means = lapply(data.frame(car.stay.df), FUN=mean)
car.stay.sd = lapply(data.frame(car.stay.df), FUN=sd)

car.combined.df <- cbind(car.stay.df,car.switch.df)

#ggplot(car.stay.df,aes(x=low))+geom_histogram(binwidth=3,fill='green')+
#  geom_histogram(binwidth=5,aes(med),fill='red')+
#  geom_histogram(binwidth=5,aes(hi),fill='blue')

ggplot(car.combined.df,aes(x=low.st))+geom_histogram(binwidth=3,fill='red')+
  geom_histogram(binwidth=5,aes(med.st),fill='red')+
  geom_histogram(binwidth=5,aes(hi.st),fill='red')+
  geom_histogram(binwidth=5,aes(low.sw),fill='blue')+
  geom_histogram(binwidth=5,aes(med.sw),fill='blue')+
  geom_histogram(binwidth=5,aes(hi.sw),fill='blue')

