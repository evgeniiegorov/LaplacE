getlibrary <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

getlibrary('MASS')    #to matrix inverse 
getlibrary('optimx')  #to extr. point estimate
getlibrary('maxLik')  #to numerical derivatives 


