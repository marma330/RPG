euclidean <- function(num1,num2){
  while(num2!=0){
    t <- num2
    num2 <- num1%%num2
    num1 <- t
  }
  return(num1)
}
