library(readxl)
library(tidyverse)
library(stringr)
library(igraph)


EDGE <- read_excel("input/Data_Tables/ToC.xlsx", sheet = "EdgeList") %>% 
  mutate(From = toupper(From), To  = toupper(To))

GRAPH <- graph_from_data_frame(d = EDGE, directed = TRUE)

plot(GRAPH)

NODE <- read_excel("input/Data_Tables/ToC.xlsx", sheet = "NodeList") %>% 
  select(NodeID, Node, NodeType) %>% 
  mutate(NodeID = toupper(NodeID) )

Node_LU <- setNames(NODE$Node, NODE$NodeID)

EDGE_Text <- EDGE %>% 
  mutate(From = Node_LU[From], To = Node_LU[To])

EDGE_Text[which(is.na(EDGE_Text$From)),]

GRAPH <- graph_from_data_frame(d = EDGE, directed = TRUE, vertices = NODE)

plot(GRAPH, mark.col = NodeType)
