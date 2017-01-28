# UW - DSC - MDA Winter 2017
# Assignment 3
# Dave Wine 8430191

# Add packages
require(ggplot2)

# Variables
doors <- c(1,2,3)
prizes <- c('Goat','Goat','Goat')
states <- c('Closed','Closed','Closed')

# Define number of games
n <- 3000

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
    cat("Player Wins a",player.prize,"\n")

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

###############

# Main Loop

# Run the same number of games with the contestant 'Staying' vs. 'Switching'.
# You could also randomize the last player choice and then sort out the wins from not, but this is equivalent and easier.
stats.stay <-game.instance("Stay",n)
stats.switch <- game.instance("Switch",n)


# Plot Results