#' @title Euclidean
#' @details greatest common divisor two numbers by euclidean algorithm
#' @description (a,b)=(b,r) if a=b*q+r and r is remaining
#' @param a A number.
#' @param b A number.
#' @return Greatest common divisor of a and b \code{x} and \code{y}.
#' @seealso \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'   

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