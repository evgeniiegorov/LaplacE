source(file = 'libraries.R')
source(file = 'laplace_estimate.R')
source(file = 'expect.R')

#
#Minimal example for each function to illustrate
#no point just technical examples
#

##estimate improper integrals with input l
laplace_estimate(n = 1, type = 0, theta0 = 1, l.b = -1, u.b = 1, l = function(theta){theta^2} ) #sqrt(pi)
laplace_estimate(n = 1, type = 0, theta0 = c(1,1), l.b = c(-1,-1), u.b = c(1,1), 
                 l = function(theta){sum(theta^2)} ) #pi

##estimate improper integral with input theta.density,h,like
theta.density <- h <- function(theta){
  return(1)
}
like <- function(theta){
  dnorm(theta, mean = 0, sd = 1)
}
laplace_estimate(n = 1, type = 1, theta0 = 1, l.b = -1, u.b = 1, h = h, 
                 theta.density = theta.density, like = like) #one 

#estimate expect value 
h <- function(theta){
  theta
}
theta.density <- function(theta){
  dnorm(theta, mean = 1, sd = 1)
}
like <- function(theta){
  dnorm(theta, mean = 1, sd = 1)
}
expect(n = 1000, theta0 = 0.5, l.b = 1/10^3, u.b = 1, h = h, theta.density = theta.density, like = like)
