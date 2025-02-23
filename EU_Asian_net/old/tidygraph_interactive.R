#install.packages("plotly")
library(igraph)
library(ggraph)
library(plotly)
library(tidygraph)

adj_aver_complete = adj_aver_after_1991
threshold <- 0
adj_matrix <- adj_aver_complete
adj_matrix[adj_matrix < threshold] <- 0

non_zero_indices <- rowSums(adj_matrix) > 0 | colSums(adj_matrix) > 0
adj_matrix <- adj_matrix[non_zero_indices, non_zero_indices]

graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

V(graph)$size <- strength(graph, mode = "all")  

tidy_graph <- as_tbl_graph(graph)

layout <- create_layout(tidy_graph, layout = "fr")  #

node_data <- layout %>%
  mutate(id = 1:n(), label = name) %>%  
  select(x, y, size, label)             

edge_data <- as.data.frame(as_edgelist(graph)) %>%
  rename(source = V1, target = V2) %>%
  left_join(node_data %>% mutate(id = label), by = c("source" = "label")) %>%
  rename(x = x, y = y) %>%
  left_join(node_data %>% mutate(id = label), by = c("target" = "label"),
            suffix = c("_source", "_target"))  

p <- plot_ly() %>%
  add_segments(
    x = ~x_source, y = ~y_source,
    xend = ~x_target, yend = ~y_target,
    data = edge_data, line = list(color = "lightgray", width = 0.5),
    hoverinfo = "none"
  ) %>%
  add_trace(
    x = ~x, y = ~y,
    type = "scatter",
    mode = "markers+text",
    text = ~label,
    hoverinfo = "text",
    marker = list(size = ~size, color = "lightblue"),
    textfont = list(size = 8, color = "black"),
    textposition = "auto",
    data = node_data
  ) %>%
  layout(
    title = "Interactive Network Graph",
    xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
    yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
    showlegend = FALSE
  )

p <- p %>%
  layout(
    xaxis = list(range = c(-3.5, 4)),  
    yaxis = list(range = c(-3.5, 4.5))   
  )
p
