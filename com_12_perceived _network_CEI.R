#Data Stedum


# Load data ---------------------------------------------------------------

adj_stedum <- read.csv2("/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/adj_stedum.csv", header= TRUE, sep = ",")
View(adj_stedum)

adj_stedum[is.na(adj_stedum)] <- 0

adj_mat <- as.matrix(adj_stedum) 


View(adj_mat)
adj_mat <- adj_mat[, -which(colnames(adj_mat) == "club")]

# Making a network graoh --------------------------------------------------

library(igraph)
g_stedum<- graph_from_adjacency_matrix(adj_mat, mode = "undirected",weighted = TRUE, diag= FALSE, add.colnames = NULL )

#check vertex attributes
vertex_attr(g_stedum)


V(g_stedum)$color <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
V(g_stedum)$size <- 8

##set edge color
E(g_stedum)$color <- rgb(0, 1, 0, 1)
E(g_stedum)$width <- sqrt(E(g_stedum)$weight)
# Set edge width based on weight

#define vertex shape
V(g_stedum)$shape <- "circle"

#Ok lets also add the club sizes

stedum_size <- read.csv2("/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/stedum_members.csv", header= TRUE, sep = ",")

# Normalize node names in the graph and data frame for matching
V(g_stedum)$name<- trimws(V(g_stedum)$name)  # Remove leading/trailing spaces
stedum_size[, 1]<- trimws(stedum_size[, 1])     # Do the same for the data frame


# Match node sizes to graph vertices
node_indices <- match(V(g_stedum)$name,stedum_size[, 1])  # Map graph node names to data frame

#replace NAs with the next valid number
for (i in seq_along(node_indices)) {
  if (is.na(node_indices[i])) {
    node_indices[i] <- i  # Assign the current index to the NA
  }
}

# Verify the result
print(node_indices)
# Add the size as an attribute to the graph
V(g_stedum)$size <- sqrt(stedum_size[node_indices, 2])
V(g_stedum)$size[is.na(V(g_stedum)$size)] <- 1  # Replace NA sizes with a default value


#highlight CEI
V(g_stedum)$color[V(g_stedum)$name == "4"] <- "magenta"
V(g_stedum)$shape[V(g_stedum)$name == "Duurzaam_Stedum_STEC"] <- "square"
V(g_stedum)$size[V(g_stedum)$name== "Duurzaam_Stedum_STEC"] <-15


layout <- layout_with_fr(g_stedum, grid = "nogrid")
layout <- layout_nicely(g_stedum)


set.seed(1)
plot(g_stedum, layout = layout_nicely) ##with isolate respondents, without respeondent labels
     
     


#replace names by numbers
id_map <- read.csv2(
  "/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/stdm_club_membership_sample.csv",
  header = TRUE,
  sep = ",",
  fileEncoding = "latin1"  # or try "windows-1252"
)


# Clean whitespace
id_map$club <- trimws(id_map$club)  

V(g_stedum)$name <- trimws(V(g_stedum)$name)

#replace node names wihth ids
# Match graph node names to the club names in the ID map
matched_ids <- id_map$id[match(V(g_stedum)$name, id_map$club)]

# Optional: handle unmatched names
unmatched <- is.na(matched_ids)
if (any(unmatched)) {
  warning("Some node names were not matched to IDs. Assigning fallback values.")
  matched_ids[unmatched] <- seq_along(matched_ids)[unmatched]
}

# Replace node names with numeric IDs
V(g_stedum)$name <- matched_ids

set.seed(1)


     
plot(
  g_stedum,
  layout = layout_nicely,
  vertex.label = V(g_stedum)$name,
  vertex.label.cex = 1,
  vertex.label.color = "black",   # Label text color
  vertex.label.font = 6           # 1 = plain, 2 = bold, 3 = italic
)

saveRDS(g_stedum, "/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/g_stedum.rds")
g_stedum<- readRDS ("/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/g_stedum.rds")





# compute centrality of CEI_12 --------------------------------------------

if ("4" %in% V(g_stedum)$name) {
  # Get the vertex ID
  v_id <- which(V(g_stedum)$name == "4")
  
  # Degree Centrality
  deg <- degree(g_stedum, v = v_id)
  
  # Closeness Centrality
  clo <- closeness(g_stedum, v = v_id, normalized = TRUE)
  
  # Betweenness Centrality
  bet <- betweenness(g_stedum, v = v_id, normalized = TRUE)
  
  # Eigenvector Centrality
  eig <- eigen_centrality(g_stedum)$vector[v_id]
  
  # Print results
  cat("Centrality measures for node '4':\n")
  cat("Degree:", deg, "\n")
  cat("Closeness:", clo, "\n")
  cat("Betweenness:", bet, "\n")
  cat("Eigenvector:", eig, "\n")
} else {
  cat("Node '4' not found in the graph.\n")
}

# Centrality measures for node '4':
#   Degree: 13 
# Closeness: 0.08923077 
# Betweenness: 0.5717406 
# Eigenvector: 0.08804579 





#highlight clubs that are connected in both
V(g_stedum)$color[V(g_stedum)$name == "26"] <- "yellow"
V(g_stedum)$color[V(g_stedum)$name == "12"] <- "yellow"
V(g_stedum)$color[V(g_stedum)$name == "3"] <- "yellow"
V(g_stedum)$color[V(g_stedum)$name == "21"] <- "yellow"
V(g_stedum)$color[V(g_stedum)$name == "25"] <- "yellow"
V(g_stedum)$color[V(g_stedum)$name == "2"] <- "yellow"
V(g_stedum)$color[V(g_stedum)$name == "26"] <- "yellow"
V(g_stedum)$color[V(g_stedum)$name == "17"] <- "yellow"
V(g_stedum)$color[V(g_stedum)$name == "33"] <- "yellow"