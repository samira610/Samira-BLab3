#' @title Dijkstra
#' @details Calculate the shortest path (d) of every other node from init_node(s) as a vector by Dijkstra algorithm
#' @description Dijkstra's algorithm marks the distance (from init node) to every other node.
#' This is done by determining the sum of the distance between an unvisited intersection and the value of the current intersection and then relabeling the unvisited intersection with this value (the sum) if it is less than the unvisited intersection's current value.
#' This algorithm therefore expands outward from the starting point, interactively considering every node that is closer in terms of shortest path distance until it reaches the destination.
#' This returns an array containing the length of the shortest path from the start node (init node) to each other node.
#' It is only guaranteed to return correct results if there are no negative edges in the graph. Positive cycles are fine.
#' @param graph A data frame.
#' @param init_node source.node from data frame.
#' @return The shortest path (d) of every other node from init_node(s) as a vector \code{distant}.
#' @seealso \url{https://en.wikipedia.org/wiki/dijkstra_algorithm}
#' @export

dijkstra <- function(graph,init_node){
  # calculate the shortest path (d) of every other node from init_node(s) as a vector
  # by dijkstra algorithm
  # v1 and v2 are the edge of graph
  # w is the weight of the edge
  stopifnot (is.data.frame(graph), is.numeric(init_node))
  n<- unique(graph$v1)
  
  d <- matrix(data = Inf, nrow = length(unique(graph$v1)), ncol = length(unique(graph$v2)))
  #creating the distance matrix d=w[v1,v2]
  for (i in seq_along(graph$v1)) {
    a<- graph$v1[i]
    b<- graph$v2[i]
    d[a,b]<-graph$w[i]
    d[a,a]<-0
  }
  
  distant <- rep(Inf, length(unique(graph$v1)))
  visited_nodes <- rep(FALSE, length(unique(graph$v2)))
  distant[init_node] <- 0
  visited_nodes[init_node]<-TRUE
  
  repeat {
  for(i in seq_along(n)) {
    for (j in seq_along(n)) {
      shortest_d<- distant[i]+d[i,j]
      if (i!=j && shortest_d< distant[j]) {
        visited_nodes[j]<- TRUE
        distant[j]<- shortest_d
      }
    }
  }
  
  if ((all(visited_nodes==TRUE))==TRUE){
    return(distant)
    break
  }
  }
}

