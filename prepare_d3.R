## Part 1: load data

# For testing
#my_nodes <- read.csv("C:/Users/Ettore/Documents/UniOfCambridge_RA/Remedies - Digital (handover)/App/Grundfos implementation/App Grundfos/data/MobileApp_Survey_Analysis_Grundfos_data_for_R-NODES.csv",header=TRUE)
#my_arcs <- read.csv("C:/Users/Ettore/Documents/UniOfCambridge_RA/Remedies - Digital (handover)/App/Grundfos implementation/App Grundfos/data/MobileApp_Survey_Analysis_Grundfos_data_for_R-ARCS.csv",header=TRUE,row.names=1,check.names=TRUE)

# my_nodes <- read.csv("data/MobileApp_Survey_Analysis_GRUNDFOS_data_for_R-NODES.csv",header=TRUE)
# my_arcs <- read.csv("data/MobileApp_Survey_Analysis_GRUNDFOS_data_for_R-ARCS.csv",header=TRUE,row.names=1,check.names=TRUE)

#my_nodes <- get(load("data/new_items.RData"))
#my_arcs <- get(load("data/new_mat.RData"))

my_nodes <- get(load("data/new_items.RData"))
my_arcs <- get(load("data/new_mat.RData"))

# Compute plot

direct_infl <- rowSums(my_arcs)
direct_depend <- colSums(my_arcs)

# DEMATEL modified
lambda = max(max(direct_infl), max(direct_depend))
A <-  my_arcs/lambda

# MICMAC
max_iter <- nrow(my_arcs)
counter <- array(0L, c(max_iter,2))
r_previous <-  rowSums(A)
d_previous <- colSums(A)
flag = 0;
for (p in 1:max_iter){
  T_micmac = A^(p+1)
  r = rowSums(T_micmac)
  d = colSums(T_micmac)
  
  r_sorted_index <- order(r)      # retrieves the indexes of sorted elements
  r_sorted <- r[r_sorted_index]
  d_sorted_index <- order(d)  
  d_sorted <- d[d_sorted_index]
  
  r_previous_sorted_index <- order(r_previous)  
  r_previous_sorted <- r_previous[r_previous_sorted_index]
  d_previous_sorted_index <- order(d_previous)  
  d_previous_sorted <- d_previous[d_previous_sorted_index]
  
  if (isTRUE(all.equal(r_sorted_index, r_previous_sorted_index)) && isTRUE(all.equal(d_sorted_index, d_previous_sorted_index))) {
    counter[p, 1] = 1;
    if (flag == 0){
      counter[p,2] = 1;
      flag = flag + 1;
    }
  }
  r_previous = r;
  d_previous = d;
}

if (sum(counter[,2]) > 0) {
  p_star <- which(!counter[,2] == 0) + 1
} else {
  p_start <- max_iter
}

T_star <- A^p_star;
rowsum_star <- rowSums(T_star)
colsum_star <- colSums(T_star)

# For MICMAC ranking
Initial_MICMAC_ranking <- order(rowSums(A) + colSums(A))
Final_MICMAC_ranking <- order(rowsum_star + colsum_star)
MICMAC_ranking <- as.numeric(c(Initial_MICMAC_ranking, Final_MICMAC_ranking))
MICMAC_ranking <- as.data.frame(cbind(rbind(as.matrix(replicate(nrow(my_nodes), "00.Initial.MICMAC.Ranking")), as.matrix(replicate(nrow(my_nodes), "01.Final.MICMAC.Rankig"))), MICMAC_ranking))
colnames(MICMAC_ranking) <- c("item_descriptor","rank")

MICMAC_ranking$Items <- c(levels(my_nodes$Item), levels(my_nodes$Item))
MICMAC_ranking$Item_names <- c(levels(my_nodes$Item_name), levels(my_nodes$Item_name))
MICMAC_ranking$Type <- c(as.matrix(my_nodes$Type), as.matrix(my_nodes$Type))
MICMAC_ranking$group <- c(as.matrix(my_nodes$Category), as.matrix(my_nodes$Category))
# check types
# sapply(MICMAC_ranking, class)
MICMAC_ranking[2] <- lapply(MICMAC_ranking[2], function(x) as.numeric(as.character(x)))
MICMAC_ranking[1] <- lapply(MICMAC_ranking[1], function(x) as.character(x))

# Classification (MICMAC)

high_low_classif <- array(0, c(max_iter,3))
colnames(high_low_classif) <- c("influence_HL","dependence_HL", "MICMAC class")
rownames(high_low_classif) <- my_nodes$Item
high_low_classif <- as.data.frame(high_low_classif)
high_depend <- colsum_star[which(colsum_star > mean(colsum_star))]
high_depend_index <- match(high_depend,colsum_star)
high_low_classif[high_depend_index,2] <- "High"
high_low_classif[-high_depend_index,2] <- "Low"
high_infl <- rowsum_star[which(rowsum_star > mean(rowsum_star))]
high_infl_index <- match(high_infl,rowsum_star)
high_low_classif[high_infl_index,1] <- "High"
high_low_classif[-high_infl_index,1] <- "Low"

high_low_classif[which(high_low_classif$influence_HL == "High" & high_low_classif$dependence_HL == "High"),3] <- "Generally dependent"
high_low_classif[which(high_low_classif$influence_HL == "Low" & high_low_classif$dependence_HL == "Low"),3] <- "Generally independent"
high_low_classif[which(high_low_classif$influence_HL == "Low" & high_low_classif$dependence_HL == "High"),3] <- "Depedendent on inter-industry supply"
high_low_classif[which(high_low_classif$influence_HL == "High" & high_low_classif$dependence_HL == "Low"),3] <- "Depedendent on inter-industry demand"

MICMACclass <- high_low_classif[,3]

# Generate graphs

responses_igraph <- graph_from_adjacency_matrix(as.matrix(my_arcs), mode = "directed", weighted = TRUE, diag = FALSE)
vertex_attr(responses_igraph, "attr1_group", index = V(responses_igraph)) <- as.vector(my_nodes$Category)
# E(responses_igraph)  #check edges summary
# V(responses_igraph)  #check vertex summary
# V(responses_igraph)$attr1_group

# generate D3 graph elements
nodes_D3 <- igraph_to_networkD3(responses_igraph, group = V(responses_igraph)$attr1_group, what = "nodes")
nodes_D3$group_index <- match(my_nodes$Category, levels(my_nodes$Category))   # for filtering nodes
nodes_D3$type <- my_nodes$Type
nodes_D3$id <- seq_len(nrow(nodes_D3)) - 1
nodes_D3$id_dynamic <- nodes_D3$id
nodes_D3$description <- my_nodes$Item_name

nodes_D3$rowsum_star <- rowsum_star
nodes_D3$colsum_star <- colsum_star
nodes_D3$MICMACclass <- MICMACclass

nodes_D3$Initial.MICMAC.Ranking <- Initial_MICMAC_ranking
nodes_D3$Final.MICMAC.Rankig <- Final_MICMAC_ranking

nodes_D3 <- nodes_D3[c("id", "id_dynamic", "name", "description", "group","group_index","type", "rowsum_star", "colsum_star", "MICMACclass", "Initial.MICMAC.Ranking", "Final.MICMAC.Rankig")]

# rescale node size for visualisation
min_scale <- 1
max_scale <- 100
visual_range <- max_scale - min_scale
test_size <- degree(responses_igraph, mode="all")
test_range <- max(test_size) - min(test_size)
test_size_rescaled <- (((test_size - min(test_size)) * visual_range) +1) / test_range
nodes_D3$size <- test_size_rescaled

edges_D3 <- igraph_to_networkD3(responses_igraph, what = "links")
edges_D3$source_dynamic <- edges_D3$source
edges_D3$target_dynamic <- edges_D3$target
edges_D3$value <- (E(responses_igraph)$weight)^4/250
for (i in 1:nrow(edges_D3)){
  edges_D3[i,"source_node_group"] <- nodes_D3[which(nodes_D3$id == edges_D3[i,"source"]), "group"]
  edges_D3[i,"target_node_group"] <- nodes_D3[which(nodes_D3$id == edges_D3[i,"target"]), "group"]
}

for (i in 1:nrow(edges_D3)){
  edges_D3[i,"source_node_type"] <- nodes_D3[which(nodes_D3$id == edges_D3[i,"source"]), "type"]
  edges_D3[i,"target_node_type"] <- nodes_D3[which(nodes_D3$id == edges_D3[i,"target"]), "type"]
}
