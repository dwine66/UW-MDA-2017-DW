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

# Define number of games
n <- 100


# Functions
###############

# Game Function

game.instance <- function(pc,n){
  #initialize counter
  result.car <- 0
  for (i in 1:n){
  doors.load
  player.guess
  host.reveal(door.car, door.guess)
  states[host.reveal] <- "Opened"
  player.choose(pc)
  player.reveal
  # then add outcome to result set
    if (player.prize == 'Car') result.car = result.car + 1
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
    host.reveal <- pick.another(door.car,3)
    # otherwise choose one that isn't the car or the one the player chose
    else host.reveal <- doors[-car,-guess]
  }
  states[host.reveal] <- "Opened"
}

# Contestant chooses from remaining doors
player.choose <- function(choice){
  doors.left=subset(doors,states[-"Opened"]) # Can only choose from doors that are not opened
  choice <- as.integer(runif(1,0,2))+1
  if (choice == 1) {
    player.choice <- choices[1] 
    else player.choice <- choices[2] # Switch
    }
  player.choice <- x
}

# Host reveals contestant door 
player.reveal <- function(){
  if (player.choice == door.car)
  
}

###############

# Main Loop

# Run the same number of games with the contestant 'Staying' vs. 'Switching'.
# You could also randomize the last player choice and then sort out the wins from not, but this is equivalent and easier.
stats.stay <-game.instance("Stay",n)
stats.switch <- game.instance("Switch",n)


# Plot Results