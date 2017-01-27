# UW - DSC - MDA Winter 2017
# Assignment 3
# Dave Wine 8430191

# Add packages
require(ggplot2)

# Variables
doors <- c('Door 1','Door 2','Door 3')
prizes <- c('Goat','Goat','Goat')
choices <- c('Stay','Switch')
states <- c('Closed','Closed','Closed')

# Functions
###############

# Pick Another - recursively chooses another number that isn't the one passed to the function
pick.another <-function(not.this.one){
  x <- as.integer(runif(1,0,3))+1 
  if(x == not.this.one) pick.another(not.this.one) else x
  
}
# Pick one door at random to hold the car
doors.load <- function(){
  door.car <- as.integer(runif(1,0,3))+1
  prizes[door.car] <- 'Car'
}

# Give the host car info

# Contestant first guess
player.guess <- function(){
  door.guess <- as.integer(runif(1,0,3))+1
  states[door.guess] <- 'Chosen'
  
}
# Host reveals door of his choice
host.reveal <-function(car,guess){
  # if the player's guess is correct, choose a goat at random
  if (guess == car) {
    door.reveal <- pick.another(door.car) 
    else door.reveal <- doors[-car,-guess]
  }
}

# Contestant chooses again
player.choose <- function(){
  choice <- as.integer(runif(1,0,2))+1
  if (choice == 1) {
    player.choice <- choices[1] 
    else player.choice <- choices[2] # Switch
    }
  player.choice <- 
}

# Host reveals contestant door 
player.reveal <- function(){
  if (player.choice == door.car)
  
}

###############

# Define number of games
n <- 100
results <- df

# Run the game
for (i in 1:n){
doors.load
player.guess
host.reveal(door.car, door.guess)
states[host.reveal] <- "Opened"
player.choose
player.reveal
}

# Plot Results