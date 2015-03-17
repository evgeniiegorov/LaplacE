#'@description Estimate integral with Laplase approx.
#'@author Evgenii Egorov
#' @param type Type of estimation. By default, function will estimate integrals of form exp(-N*l).
#' and wait l(x) function as input. If type = 1, function will esimate integrals of form h*theta.density*like, 
#' where h,theta.density,like are functions of x.
#' @param n Constant N in integrand exp(-N*l) or if type = 1 number of observations.
#' @param theta0 Initial point to find modal value of l(x), number or vector of numbers.
#' @param l.b Low bound of set where to search modal value of l(x), number or vector of numbers with dim = dim(x)
#' @param u.b Upper bound of set where to search modal value of l(x), 
#' number or vector of numbers with dim = dim(x).
#' @param l Function in integrand exp(-N*l), function of x.
#' @param h,theta.density,like Functions of x, if type = 1.
#' @return Value of integral and modal point x.
laplace_estimate <- function(type = 0,
                             n,
                             theta0, l.b, u.b,
                             l,
                             h,theta.density,like){  
  if (type == 1){
    l <- function(theta){
     -1*(log(h(theta)) + log(theta.density(theta)) + log(like(theta)))/n
    }
  }
  
  est.normf <- function(n,l,theta0,l.b,u.b){
    theta.hat <- as.numeric(optimx(fn = l, 
                      par = theta0, 
                      lower = l.b, upper = u.b, 
                      method = 'L-BFGS-B')[1,1:length(theta0)])
  
    diff2 <- numericHessian(f = l, t0 = theta.hat)
    result <- exp(-n*l(theta.hat)) * (2*pi)^(length(theta.hat)/2) * (1/n)^(length(theta.hat)/2) * sqrt(det(ginv(diff2)))
    list(result = result,
        modal = theta.hat)
  }
  return(est.normf(n,l,theta0,l.b,u.b))
}

