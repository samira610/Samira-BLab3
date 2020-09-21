#' @title Dijkstra
#' @details Calculate the shortest path (d) of every other node from init_node(s) as a vector by Dijkstra algorithm
#' @description Dijkstra's algorithm marks the distance (from init node) to every other node.
#' This is done by determining the sum of the distance between an unvisited intersection and the value of the current intersection and then relabeling the unvisited intersection with this value (the sum) if it is less than the unvisited intersection's current value.
#' This algorithm therefore expands outward from the starting point, interactively considering every node that is closer in terms of shortest path distance until it reaches the destination.
#' This returns an array containing the length of the shortest path from the start node (init node) to each other node.
#' It is only guaranteed to return correct results if there are no negative edges in the graph. Positive cycles are fine.
#' @param graph A data frame.
#' @param init_node source.node from data fram.
#' @return The shortest path (d) of every other node from init_node(s) as a vector \code{distant}.
#' @seealso \url{https://en.wikipedia.org/wiki/dijkstra_algorithm}
#' @export
dijkstra <- function(graph,init_node){
  # calculate the shortest path (d) of every other node from init_node(s) as a vector
  # by dijkstra algorithm
  # v1 and v2 are the edge of graph
  # w is the weight of the edge
  stopifnot (is.data.frame(graph), is.numeric(init_node))
    s <- 0
    distant<-0
    v1 <- graph[,1]
    v2 <- graph[,2]
    w <- graph[,3]
    distant = rep(Inf, length(graph[,1]))
    visited_nodes = rep(FALSE, length(graph[,1]))
    distant[init_node] = 0
    repeat{
      shortest_distant = Inf
      shortest_index = -1
      for(i in seq_along(distant)) {
        if(distant[i] < shortest_distant && !visited_nodes[i]){
          shortest_distant = distant[i]
          shortest_index = i
        }
      }
      cat("Visiting node ", shortest_index, " with current distance ", shortest_distant, "\n")
      
      if(shortest_index == -1){
        return (distant)
      }
      for(i in seq_along(graph[shortest_index,])) {
        if(graph[shortest_index,i] != 0 && distant[i] > distant[shortest_index] + graph[shortest_index,i]){
          # ...Save this path as new shortest path.
          distant[i] = distant[shortest_index] + graph[shortest_index,i]
          cat("Updating distance of node ", i, " to ", distances[i], "\n")
        }
        # Lastly, note that we are finished with this node.
        visited_nodes[shortest_index] = TRUE
        cat("Visited nodes: ", visited_nodes, "\n")
        cat("Currently lowest distances: ", distant, "\n")
      }
    }
}
#1.1.2 dijkstra()
