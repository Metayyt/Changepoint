library(igraph)
library(visNetwork)

threshold <- 0.2
adj_matrix <- adj_aver_complete
adj_matrix[adj_matrix < threshold] <- 0

#non_zero_indices <- rowSums(adj_matrix) > 0 | colSums(adj_matrix) > 0
#adj_matrix <- adj_matrix[non_zero_indices, non_zero_indices]

graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

if (is.null(V(graph)$name) || all(V(graph)$name == "")) {
  V(graph)$name <- as.character(1:vcount(graph))
}

nodes <- data.frame(
  id = V(graph)$name,
  label = V(graph)$name,
  size = strength(graph, mode = "all"),
  title = paste("Node:", V(graph)$name, "<br>Importance:", strength(graph, mode = "all"))
)

edges <- data.frame(
  from = as.character(ends(graph, E(graph))[, 1]),
  to = as.character(ends(graph, E(graph))[, 2]),
  value = ifelse(is.na(E(graph)$weight) | E(graph)$weight <= 0, 1, E(graph)$weight)
)

visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEdges(smooth = TRUE,  scaling = list(min = 0.1, max = 1)) %>%  
  visNodes(scaling = list(min = 20, max = 100)) %>%
  visLayout(randomSeed = 123) %>%
  visLegend() %>%
  visInteraction(hover = TRUE) %>%
  visPhysics(
    solver = "forceAtlas2Based",  #
    stabilization = list(iterations = 200),  
    maxVelocity = 10, 
    timestep = 0.5     
  )
