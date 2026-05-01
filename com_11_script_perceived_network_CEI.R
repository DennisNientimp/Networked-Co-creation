#Data Kloosterburen


# Load data ---------------------------------------------------------------

adj_KB <- read.csv2("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/perc_data/KB_adj.csv", header= TRUE, sep = ",")


adj_KB[is.na(adj_KB)] <- 0 # make Nas 0s
adj_KB[is.na(adj_KB) | adj_KB == ""] <- 0 # make empty cells 0s

adj_KB[adj_KB == "?"] <- 1 #for nnow I have soem conenctions in there without a value and therefore a ? I replace those by 1


adj_KB <- as.matrix(adj_KB) 


View(adj_KB)
adj_KB <- adj_KB[, -which(colnames(adj_KB) == "Club")]


# Making a network graoh --------------------------------------------------
library("igraph")
g_KB<- graph_from_adjacency_matrix(adj_KB, mode = "undirected",weighted = TRUE, diag= FALSE, add.colnames = NULL )

#check vertex attributes
vertex_attr(g_KB)

V(g_KB)$color <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
V(g_KB)$size <- 8

##set edge color
E(g_KB)$color <- "#FFB3B3"

E(g_KB)$width <- sqrt(E(g_KB)$weight) #this squared tranformation makes the difference between large (100) and small (1) weigths less extreme

# Set edge width based on weight

#define vertex shape
V(g_KB)$shape <- "circle"



#highlight CEI
V(g_KB)$color[V(g_KB)$name == "PEP_5"] <- "#90EE90"
V(g_KB)$shape[V(g_KB)$name == "PEP_5"] <- "square"
V(g_KB)$size[V(g_KB)$name== "PEP_5"] <-15

V(g_KB)$label <- V(g_KB)$name



#Ok lets also add the club sizes

KB_size <- read.csv2("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/perc_data/KB_clubs_members.csv", header= TRUE, sep = ",")

# Normalize node names in the graph and data frame for matching
V(g_KB)$name<- trimws(V(g_KB)$name)  # Remove leading/trailing spaces
KB_size[, 1]<- trimws(KB_size[, 1])     # Do the same for the data frame


# Match node sizes to graph vertices
node_indices <- match(V(g_KB)$name,KB_size[, 1])  # Map graph node names to data frame

#replace NAs with the next valid number
for (i in seq_along(node_indices)) {
  if (is.na(node_indices[i])) {
    node_indices[i] <- i  # Assign the current index to the NA
  }
}

# Verify the result
print(node_indices)
# Add the size as an attribute to the graph
V(g_KB)$size <- 0.2*(sqrt((KB_size[node_indices, 2])))
V(g_KB)$size[is.na(V(g_KB)$size)] <- 1  # Replace NA sizes with a default value


layout <- layout_with_fr(g_KB, grid = "nogrid")
layout <- layout_nicely(g_KB)

layout <- layout_with_fr(g_KB, niter = 10000, area = vcount(g_KB)^4, repulse = TRUE) 

# Increase iterations and space
set.seed(1)
plot(g_KB, layout = layout)


#good for an overview
layout <- layout_in_circle(g_KB)
plot(g_KB, layout = layout)


set.seed(1)
plot(g_KB, layout = layout_nicely)

saveRDS(g_KB, "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/perc_data/g_KB.rds")

g_KB <- readRDS("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/perc_data/g_KB.rds")





# remove isolate
# Identify isolated vertices (nodes with degree 0)
isolates <- V(g_KB)[degree(g_KB) == 0]

# Remove isolated vertices from the graph
g_KB_no_iso <- delete_vertices(g_KB, isolates)


set.seed(1)
plot(g_KB_no_iso, layout = layout_nicely)



#replace names by numbers

id_map <- read.csv2(
  "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/perc_data/KB_clubs_members_per_samp.csv",
  header = TRUE,
  sep = ",",
  fileEncoding = "latin1"  # or try "windows-1252"
)


# Clean whitespace
id_map$club <- trimws(id_map$ï..club)  

V(g_KB)$name <- trimws(V(g_KB)$name)

#replace node names wihth ids
# Match graph node names to the club names in the ID map
matched_ids <- id_map$id[match(V(g_KB)$name, id_map$club)]

# Optional: handle unmatched names
unmatched <- is.na(matched_ids)
if (any(unmatched)) {
  warning("Some node names were not matched to IDs. Assigning fallback values.")
  matched_ids[unmatched] <- seq_along(matched_ids)[unmatched]
}

# Replace node names with numeric IDs
V(g_KB)$name <- matched_ids

set.seed(1)
plot(
  g_KB,
  layout = layout_nicely,
  vertex.label = V(g_KB)$name,
  vertex.label.cex = 1.2,
  vertex.label.color = "blue",   # Label text color
  vertex.label.font = 6           # 1 = plain, 2 = bold, 3 = italic
)


#making PEP 5 visible
V(g_KB)$name[V(g_KB)$name == "14"] <- "CEI_11"
V(g_KB)$shape[V(g_KB)$name == "CEI_11"] <- "square"
V(g_KB)$size[V(g_KB)$name== "CEI_11"] <-15


V(g_KB)$shape[V(g_KB)$name == "14"] <- "square"
V(g_KB)$size[V(g_KB)$name== "14"] <-15
set.seed(7)
plot(g_KB, vertex.label = V(g_KB)$name,vertex.label.cex = 0.8,layout= layout_nicely)





# centrality of CEI_11 ----------------------------------------------------

# Ensure node "14" exists
if ("14" %in% V(g_KB)$name) {
  # Get the vertex ID
  v_id <- which(V(g_KB)$name == "14")
  
  # Degree Centrality
  deg <- degree(g_KB, v = v_id)
  
  # Closeness Centrality
  clo <- closeness(g_KB, v = v_id, normalized = TRUE)
  
  # Betweenness Centrality
  bet <- betweenness(g_KB, v = v_id, normalized = TRUE)
  
  # Eigenvector Centrality
  eig <- eigen_centrality(g_KB)$vector[v_id]
  
  # Print results
  cat("Centrality measures for node '14':\n")
  cat("Degree:", deg, "\n")
  cat("Closeness:", clo, "\n")
  cat("Betweenness:", bet, "\n")
  cat("Eigenvector:", eig, "\n")
} else {
  cat("Node '14' not found in the graph.\n")
}

# Centrality measures for node '14':
#   Degree: 17 
# Closeness: 0.2204724 
# Betweenness: 0.5293651 
# Eigenvector: 0.009423181 



#highlight clubs that are connected in both
V(g_KB)$color[V(g_KB)$name == "23"] <- "yellow"
V(g_KB)$color[V(g_KB)$name == "30"] <- "yellow"
V(g_KB)$color[V(g_KB)$name == "16"] <- "yellow"
V(g_KB)$color[V(g_KB)$name == "9"] <- "yellow"
V(g_KB)$color[V(g_KB)$name == "5"] <- "yellow"

set.seed(7)
plot(g_KB, layout= layout_nicely)


saveRDS(g_KB, "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/perc_data/g_KB_numbered_anonymous.rds")




V(g_com_11_club)$color[V(g_com_11_club)$name == "23"] <- "yellow"
V(g_com_11_club)$color[V(g_com_11_club)$name == "30"] <- "yellow"
V(g_com_11_club)$color[V(g_com_11_club)$name == "16"] <- "yellow"
V(g_com_11_club)$color[V(g_com_11_club)$name == "9"] <- "yellow"
V(g_com_11_club)$color[V(g_com_11_club)$name == "5"] <- "yellow"


pdf("/Users/dennisnientimp/Desktop/g_com_11_club.pdf", width = 8, height = 6)
set.seed(3)
plot(g_com_11_club, layout= layout_nicely)
dev.off()


g_com_11_club <- readRDS("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/g_com_11_club.rds")

# club on mode ------------------------------------------------------------

onemode <- bipartite_projection(g_com_11)
g_com_11_club <- onemode$proj2  

g_com_11_club <- g_com_11_club
#lest grab the matrix 
mat <- as_adjacency_matrix(graph = g_com_11_club, attr = "weight", sparse = F)
head(mat)

saveRDS(mat, "/Users/dennisnientimp/Desktop/")


# Assign purple color with 90% opacity to node named "14"
V(g_com_11_club)$color[V(g_com_11_clubh)$name == "14"] <- rgb(0.5, 0, 0.5, 0.9)


pdf("/Users/dennisnientimp/Desktop/networks/g_com_11_club.pdf", width = 8, height = 6)
set.seed(1)
plot(g_com_11_club, layout = layout_with_fr)
dev.off()


#lets look for the nodes exactly two steps away
which(distances(g_com_11_club)["CEI_11", ] == 2)

library(igraph)

# Get the vertex names exactly 2 steps away from "CEI_11"
two_step_nodes <- names(which(distances(g_com_11_club)["CEI_11", ] == 2))

# Create a default color vector (e.g., light gray)
v_colors <- rep("lightgray", vcount(g_com_11_club))

# Set yellow for the two-step nodes
v_colors[V(g_com_11_club)$name %in% two_step_nodes] <- "yellow"

# Plot with custom colors

set.seed(1)
plot(g_com_11_club, vertex.color = v_colors, vertex.size = 10)







# ptp one mode ------------------------------------------------------------


onemode <- bipartite_projection(g_com_11)
g_com_11_ptp <- onemode$proj1  
#lest grab the matrix 
mat <- as_adjacency_matrix(graph = g_com_11_ptp, attr = "weight", sparse = F)
head(mat)
saveRDS(mat, "/Users/dennisnientimp/Desktop/networks/g_com_11_ptp.rds")

#now lets plot it
# Assign purple color to initiators
V(g_com_11_ptp)$color[V(g_com_11_ptph)$name == "103"] <- rgb(0.5, 0, 0.5, 0.9)
V(g_com_11_ptp)$color[V(g_com_11_ptph)$name == "104"] <- rgb(0.5, 0, 0.5, 0.9)
V(g_com_11_ptp)$color[V(g_com_11_ptph)$name == "105"] <- rgb(0.5, 0, 0.5, 0.9)
V(g_com_11_ptp)$color[V(g_com_11_ptph)$name == "106"] <- rgb(0.5, 0, 0.5, 0.9)
V(g_com_11_ptp)$color[V(g_com_11_ptph)$name == "107"] <- rgb(0.5, 0, 0.5, 0.9)
V(g_com_11_ptp)$color[V(g_com_11_ptph)$name == "108"] <- rgb(0.5, 0, 0.5, 0.9)



pdf("/Users/dennisnientimp/Desktop/networks/g_com_11_ptptest.pdf", width = 8, height = 6)
set.seed(1)
plot(g_com_11_ptp, layout = layout_with_fr)
dev.off()


