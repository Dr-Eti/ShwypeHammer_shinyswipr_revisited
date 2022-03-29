# transform swipe results (csv) for the backend to consume

# loads nodes info and results
nodes <- read.csv("data/test_data_20220321.csv")
swipe_list <- read.csv(file ="data/results_202203260104.csv", header = FALSE)

# creates matrix of zeros
new_mat = matrix(0, length(nodes$node_ID), length(nodes$node_ID))

# rename rows and columns
rownames(new_mat) <-nodes$node_ID 
colnames(new_mat) <-nodes$node_ID

# loop through swipe results and store into new_mat
for (row in 1:nrow(swipe_list)) {
  # IF rownames are node_ID
  source <- which(nodes$node_label==swipe_list["V2"][row,]) 
  target <- which(nodes$node_label==swipe_list["V3"][row,]) 
  
  # IF rownames are node_label
  # source <- swipe_list["V2"][row,]
  # target <- swipe_list["V3"][row,]
  val <- swipe_list["V5"][row,]
  new_mat[source,target] <- val
}

# now preprocess the nodes into a format for backend to consume
new_items <- data.frame(nodes$node_ID, nodes$node_label)
colnames(new_items) <- c("Item", "Item_name")
new_items$Type <- "Strategic.Design" #dummy
new_items$Category <- "Flexible and automated production" #dummy


# save both new_mat and new_items 
save(new_mat, file = "data/new_mat.RData")
save(new_items, file = "data/new_items.RData")

# csv_items <- "new_items.csv"
# csv_mat <- "new_mat.csv"
# write.table(new_mat,  
#             file=csv_mat, 
#             sep=',', 
#             row.names=T, 
#             col.names=T)
# write.table(new_items,  
#             file=csv_items, 
#             sep=',', 
#             row.names=F, 
#             col.names=T)

