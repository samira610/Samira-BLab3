#' @title euclidean
#' @details greatest common divisor two numbers by euclidean algorithm.
#' @description In this algorithm, the quotient and remainder are found by repeated subtraction, until the last remainder that is grater than zero. 
#' Euclidean division reduces all the steps between two exchanges into a single step.
#' @param a A number.
#' @param b A number.
#' @return Greatest common divisor of a and b \code{a} and \code{b}.
#' @seealso \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export

euclidean<-function(a,b){
  #greatest common divisor a and b by euclidean algorithm (a,b)=(b,r) if a=b*q+r
  #r is remaining
  x<-a
  y<-b
  while(y!=0) {
    r<- x%%y
    x<-y
    y<-r
  }
  return(x)
}