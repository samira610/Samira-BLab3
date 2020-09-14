#1.1.1 euclidean()
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
}#1.1.1 euclidean()

#1.1.2 dijkstra()
dijkstra <- function(graph_v1_v2_w,init_node){
  # calculate the shortest path (d) of every other node from init_node(s) as a vector
  # by dijkstra algorithm
  # v1 and v2 are the edge of graph
  # w is the weight of the edge
  if (is.data.frame(graph_v1_v2_w)==TRUE && is.numeric(init_node)==TRUE){
 s <- 0
 distant(s)<-0
 v1 <- graph_v1_v2_w[,1]
 v2 <- graph_v1_v2_w[,2]
 w <- graph_v1_v2_w[,3]
 
 
}}#1.1.2 dijkstra()
