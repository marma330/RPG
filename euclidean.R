#' Greatest common divisor
#' 
#' @param num1 is a number
#' @param num2 is a number
#' @return the Greatest common divisor between two numbers 
#' @references 
#' https://en.wikipedia.org/wiki/Euclidean algorithm
#' 







euclidean <- function(num1,num2){
  while(num2!=0){
    t <- num2
    num2 <- num1%%num2
    num1 <- t
  }
  return(num1)
}
