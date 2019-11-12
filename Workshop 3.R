# Leo & Chumi's Workshop #3 

rm(list=ls())  # Clears workspace
library(emdbook)
library(ggplot2)

D<-10^-6         # Diffusion coefficient (in m^2/s)
dt<-1e-4         # Time step (in seconds)
total_time<-100  # Total time that simulation will run (in seconds)
dots_sample<-100 # How many points we want to test

distance = function(x,y)(sqrt((x-0)^2 + (y-0)^2)) # Setting up our Distance Function, sqrt(x^2 + y^2)

calculations = function(dt, D, total_time, dots_sample){
  
  dots_x<-rep(0,dots_sample)   # Creates 100 molecules to follow (x component of position)
  dots_y<-rep(0,dots_sample)   # Creates 100 molecules to follow (y component of position)
  dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.
  n = total_time/dt # Sets up a variable for our for loop
  z = rep(0, n) # Creates an empty vector to store our average distances in
  
  for (i in 1:n) {
    
    distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
    disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
    
    anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
    angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
    
    dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
    dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
    message(mean(distance(dots_x,dots_y))) # Messages the mean distance so I know everything is running okay
    z[i] = mean(distance(dots_x,dots_y)) # Stores the mean distance in our empty vector, Z, at the spot [i] in the vector
  }
  return(mean(distance(dots_x,dots_y))) #returns the mean distance so everything comes into the external enviroment
  
  # return(z) 
  plot(z) # creates a model of how it all changed over time if you return(z) instead of the mean distance (Question 4)
  
} #Setting up our Calculations Function

dt = lseq(1e-6, 1e-1, 10) # Creates a sequence, labeled dt, that contains the 10 different time steps we want to test.
dt_test = rep(0,length(dt)) # Creates an empty vector to store our results for Time Steps
for (p in 1:length(dt)) {
  
  dt_test[p] = calculations(dt[p], 10^-6, 1, 100) # Running our Calculations function with our various time steps
  
} # Our for loop to test the different Time Steps (Question 1)

dt_data = data.frame(dt, dt_test) # Graphing the results of testing various different Time Steps (Question 3)
ggplot(dt_data, aes(x = dt, y = dt_test)) + geom_point() +
  labs(title = "Time Step (In Seconds) vs Average Distance", x= "Time Step (sec)", y= "Average Distance") +
  theme(plot.title = element_text(size=14, face="bold.italic"))

D = lseq(10^-6, 10, 5)  # Creates a sequence, labeled D, that contains the 5 different diffusion coefficients we want to test.
D_test = rep(0,length(D)) # Creates an empty vector to store our results for Diffusion Coefficients
for(s in 1:length(D)) {
  
  D_test[s] = calculations(1e-4, D[s], 1, 100)  # Running our Calculations function with our various Diffusion Coefficients
  
} # Our for loop to test the different Diffusion Coefficients (Question 2)

D_data = data.frame(D, D_test) # Graphing the results of testing various different Diffusion Coefficients (Question 3)
ggplot(dt_data, aes(x = D, y = D_test)) + geom_point() +
  labs(title = "Diffusion Coefficient vs Average Distance", x= "Diffusion Coefficient", y= "Average Distance") +
  theme(plot.title = element_text(size=14, face="bold.italic"))



