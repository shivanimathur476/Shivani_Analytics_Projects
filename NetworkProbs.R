install.packages("igraph")
library(igraph)

edgelist <- data.frame(
  from = c(1,1,2,2,3,5,6),
  to = c(2,6,5,3,4,4,5),
  capacity = c(6,5,4,2,3,8,7))
g <- graph_from_data_frame(as.data.frame(edgelist))

E(g)$capacity <- edgelist$capacity


plot(g, edge.label = E(g)$capacity)

max_flow(g, source=V(g)["1"], target=V(g)["4"])
install.packages("optrees")
library(optrees)
library(igraph)

nodes= 1:7
E = matrix( c(1,2,5,1,3,7,1,4,4,2,3,1,2,5,9,3,4,2,3,6,5,3,5,4,4,7,9,6,7,6,6,5,1),byrow= TRUE, ncol = 3)
g1 <- graph_from_data_frame(as.data.frame(E))
plot(g1, edge.label = E(g)$capacity)


getShortestPathTree(nodes,E,algorithm = 'Dijkstra',directed = TRUE)



library(igraph)
library(optrees)
nodes= 1:6
E = matrix( c(1,2,6,1,6,5,2,5,4,2,3,2,3,4,3,6,5,7,5,4,8),byrow= TRUE, ncol = 3)
colnames(E) <- c("from", "to", "capacity")
g1 <- graph_from_data_frame(as.data.frame(E))
plot(g1, edge.label = E(g)$capacity)

getMinimumSpanningTree()
getShortestPathTree(nodes,E,algorithm = 'Dijkstra',directed = TRUE)
maxFlowFordFulkerson(nodes,E, directed = TRUE,source.node = 1, sink.node = 5)


-------------------------------------------
  
  library(optrees)
nodes= 1:7
E = matrix( c(1,2,5,1,3,7,1,4,4,2,3,1,2,5,9,3,4,2,3,6,5,3,5,4,4,7,9,6,7,6,6,5,1),byrow= TRUE, ncol = 3)
getMinimumSpanningTree(nodes, E ,algorithm = 'Boruvka')
getMinimumSpanningTree(nodes, E ,start.node= 7, algorithm = 'Boruvka')
getMinimumSpanningTree(nodes, E ,algorithm = 'Kruskal')
getMinimumSpanningTree(nodes, E ,algorithm = 'Prim')
