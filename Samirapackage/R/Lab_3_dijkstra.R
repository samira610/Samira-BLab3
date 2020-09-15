#' @title Dijkstra
#' @details Calculate the shortest path (d) of every other node from init_node(s) as a vector by Dijkstra algorithm
#' @description (a,b)=(b,r) if a=b*q+r and r is remaining
#' @param graph_v1_v2_w A data frame.
#' @param init_node source.node from data fram.
#' @return The shortest path (d) of every other node from init_node(s) as a vector \code{x} and \code{y}.
#' @seealso \url{https://en.wikipedia.org/wiki/dijkstra_algorithm}
#' @export
dijkstra <- function(graph_v1_v2_w,init_node){
  # calculate the shortest path (d) of every other node from init_node(s) as a vector
  # by dijkstra algorithm
  # v1 and v2 are the edge of graph
  # w is the weight of the edge
  if (is.data.frame(graph_v1_v2_w)==TRUE && is.numeric(init_node)==TRUE){
    s <- 0
    distant<-0
    v1 <- graph_v1_v2_w[,1]
    v2 <- graph_v1_v2_w[,2]
    w <- graph_v1_v2_w[,3]
    
    
  }}#1.1.2 dijkstra()
