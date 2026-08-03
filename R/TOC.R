library(readxl)
library(dplyr)
library(igraph)

EDGE <- read_excel("input/ToC.xlsx", sheet = "EdgeList") 

GRAPH <- graph_from_data_frame(d = EDGE, directed = TRUE)

plot(GRAPH)

