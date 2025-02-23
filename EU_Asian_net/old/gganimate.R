#install.packages("gganimate")
library(igraph)
library(ggraph)
library(tidygraph)

X=graphs[81:(80+cid +4),,]
adj_a = X[1,,]
for(i in 2:(dim(X)[1])){
  adj_a = adj_a + X[i,,]
}
adj_aver = adj_a / (dim(X)[1])
adj_matrix_pre1991 = adj_a / (dim(X)[1])

X=graphs[(80+cid+5):dim(graphs)[1],,]
adj_a = X[1,,]
for(i in 2:(dim(X)[1])){
  adj_a = adj_a + X[i,,]
}
adj_aver = adj_a / (dim(X)[1])
adj_matrix_post1991 = adj_a / (dim(X)[1])


library(gganimate)
library(igraph)
library(ggraph)

graph_pre1991 <- graph_from_adjacency_matrix(adj_matrix_pre1991, mode = "undirected", weighted = TRUE)
graph_post1991 <- graph_from_adjacency_matrix(adj_matrix_post1991, mode = "undirected", weighted = TRUE)

layout_pre <- layout_with_fr(graph_pre1991)
layout_post <- layout_with_fr(graph_post1991)

layout_pre <- as.data.frame(layout_pre)
layout_post <- as.data.frame(layout_post)
layout_pre$time <- "Before 1991"
layout_post$time <- "After 1991"

layout_combined <- rbind(
  cbind(layout_pre, id = rownames(layout_pre)),
  cbind(layout_post, id = rownames(layout_post))
)

p <- ggplot(layout_combined, aes(x = V1, y = V2, group = id)) +
  geom_point(aes(color = time), size = 4) +
  geom_text(aes(label = id), vjust = -1) +
  transition_states(time, transition_length = 2, state_length = 1) +
  labs(title = "Network Evolution: {closest_state}")

animate(p, nframes = 50, renderer = gifski_renderer())
