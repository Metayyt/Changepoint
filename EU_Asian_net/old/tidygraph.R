#install.packages("visNetwork")
library(igraph)
library(ggraph)
library(tidygraph)

X=graphs[81:(80+cid +4),,]
adj_a = X[1,,]
for(i in 2:(dim(X)[1])){
  adj_a = adj_a + X[i,,]
}
adj_aver = adj_a / (dim(X)[1])
adj_aver_pre_1991 = adj_a / (dim(X)[1])

X=graphs[(80+cid+5):dim(graphs)[1],,]
adj_a = X[1,,]
for(i in 2:(dim(X)[1])){
  adj_a = adj_a + X[i,,]
}
adj_aver = adj_a / (dim(X)[1])
adj_aver_after_1991 = adj_a / (dim(X)[1])


adj_aver_complete = adj_aver_pre_1991  
threshold <- 0
adj_matrix <- adj_aver_complete
adj_matrix[adj_matrix < threshold] <- 0
non_zero_indices <- rowSums(adj_matrix) > 0 | colSums(adj_matrix) > 0
adj_matrix <- adj_matrix[non_zero_indices, non_zero_indices]
graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

V(graph)$size <- strength(graph, mode = "all") 

tidy_graph <- as_tbl_graph(graph)

p = ggraph(tidy_graph, layout = "fr") +  
  geom_edge_link(aes(width = weight), color = "gray") + 
  geom_node_point(aes(size = size), color = "blue") +    #
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +      
  scale_edge_width(range = c(0, 0.1)) +                 
  scale_size_continuous(range = c(2, 10)) +              
  theme_minimal() +
 
  labs(title = "Network Graph", size = "Node Importance", edge_width = "Edge Weight")
  theme(panel.background = element_rect(fill = "lightblue", color = NA))
p
p <- p  + xlim(-5, 2) + ylim(-1, 5)
p
