#1.1.1 euclidean()
euclidean<-function(a,b){
  #greatest common divisor a and b by euclidean algorithm (a,b)=(b,r) if a=bq+r
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
