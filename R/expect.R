#'@description Esitmate moments of $g(\theta)$ with trespect to posterior density
#'@author Egorov Evgenii
#'@param n Number of observations 
#'#' @param theta0 Initial point to find modal value of l(x), number or vector of numbers.
#' @param l.b Low bound of set where to search modal value of l(x), number or vector of numbers with dim = dim(x)
#' @param u.b Upper bound of set where to search modal value of l(x), 
#' number or vector of numbers with dim = dim(x).
#' @param h,theta.density,like Functions of x, if type = 1.
#' @return Expected value: E(h(x)|Y^{n}) and normalize constant for posterior density  

expect <- function(n,
                   theta0,l.b,u.b,
                   h,theta.density,like){
  one <- function(theta){
    return(1)
  }
  denum <- laplace_estimate(n = n, theta0 = theta0, l.b = l.b, u.b = u.b, type = 1, h = one, 
                            theta.density = theta.density,like = like)
  num <- laplace_estimate(n = n, theta0 = theta0, l.b = l.b, u.b = u.b,
                          type = 1, h = h, theta.density = theta.density,like = like)
  return(list(
    expect = num$result/denum$result,
    norm.constant = denum$result
  ))
}

