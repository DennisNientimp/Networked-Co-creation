# Merging initiatives and sampled network ---------------------------------




g_com_12_club<- readRDS("/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/empirical data stdm/g_com_12_club.rds")
g_stedum <- readRDS("/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/g_stedum.rds")

#new script
library(igraph)


# Step 1: Tag edge sources
E(g_stedum)$source <- "ST"
E(g_com_12_club)$source <- "com_12"




# old method --------------------------------------------------------------



# Find the vertex with name "CEI_11" and rename it to "14"
V(g_com_12_club)$name[V(g_com_12_club)$name == "CEI_12"] <- "4"
V(g_com_12_club)$name <- gsub(" ", "", V(g_com_12_club)$name)
# Extract numeric part from each name and assign it back
V(g_com_12_club)$name <- gsub("[^0-9]", "", V(g_com_12_club)$name)


# Step 2: Extract edge lists with sources
df_ST <- as_data_frame(g_stedum, what = "edges")
df_com_12 <- as_data_frame(g_com_12_club, what = "edges")

# Get common columns
common_cols <- intersect(names(df_ST), names(df_com_12))

# Subset both data frames to common columns
df_ST_aligned <- df_ST[, common_cols]
df_com_12_aligned <- df_com_12[, common_cols]

# Step 3: Combine edge lists
df_combined <- rbind(df_ST_aligned , df_com_12_aligned)

# Step 4: Rebuild graph from combined edge list
g_com_12_merged <- graph_from_data_frame(df_combined, directed = FALSE)

# Step 5: Optional simplification
g_com_12_merged <- simplify(g_com_12_merged, remove.multiple = FALSE, remove.loops = FALSE)

# Step 7: Set uniform edge width
E(g_com_12_merged)$width <- 2

# Step 8: Style vertices
V(g_com_12_merged)$size <- 7
V(g_com_12_merged)$color <- "grey"
V(g_com_12_merged)[4]$color <- "green"  # Highlight node 14

# Step 9: Style edges (ignore KB+com_11)
E(g_com_12_merged)$color <- ifelse(grepl("ST", E(g_com_12_merged)$source), "blue", "red")



# Step 10: Plot
plot(g_com_12_merged,
     vertex.color = V(g_com_12_merged)$color,
     vertex.size = V(g_com_12_merged)$size,
     edge.color = E(g_com_12_merged)$color,
     edge.width = E(g_com_12_merged)$width)

# Step 11: Add legend (no magenta)
legend("topright", 
       legend = c("Duuzaam Stedum", "Steekproef"),
       col = c("blue", "red"),
       lty = 1, 
       lwd = 4,
       bty = "n",
       cex = 0.8)


saveRDS(g_com_12_merged,'/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/merged network/g_com_12_merged.rds' )

g_com_12_merged <-readRDS('/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/merged network/g_com_12_merged.rds')

dev.off()




# new method ------------------------------



#new script
library(igraph)


# Step 1: Tag edge sources
E(g_stedum)$source <- "stdm"
E(g_com_12_club)$source <- "com_12"


# Find the vertex with name "CEI_11" and rename it to "14"
V(g_com_12_club)$name[V(g_com_12_club)$name == "CEI_12"] <- "4"



V(g_com_12_club)$name <- gsub(" ", "", V(g_com_12_club)$name)
# Extract numeric part from each name and assign it back
V(g_com_12_club)$name <- gsub("[^0-9]", "", V(g_com_12_club)$name)

library(igraph)
# Step 2: Extract edge lists with sources
df_stdm <- igraph::as_data_frame(g_stedum, what = "edges")
df_com <- igraph::as_data_frame(g_com_12_club, what = "edges")

#comobine them

df_stdm  <- igraph::as_data_frame(g_stedum, what = "edges")  %>%
  mutate(from = as.character(from),
         to   = as.character(to),
         source = "stdm")

df_com <- igraph::as_data_frame(g_com_12_club, what = "edges") %>%
  mutate(from = as.character(from),
         to   = as.character(to),
         source = "com")

df_combined <- bind_rows(df_stdm, df_com)


#merge dublicates
library(dplyr)

df_collapsed <- df_combined %>%
  # Create an unordered vertex-pair ID so 14–30 and 30–14 are treated the same
  mutate(pair = paste(pmin(from, to), pmax(from, to), sep = "_")) %>%
  
  group_by(pair) %>%
  # If a pair has both KB and com, keep one row and mark as "double"
  mutate(
    has_stdm  = any(source == "stdm", na.rm = TRUE),
    has_com = any(source == "com", na.rm = TRUE),
    source  = ifelse(has_stdm & has_com, "double", source)
  ) %>%
  # Keep only one row per pair if it's a "double", otherwise keep all
  filter(!(source == "double" & duplicated(pair))) %>%
  ungroup() %>%
  select(-pair, -has_stdm, -has_com)

df_with_flags <- df_combined %>%
  mutate(pair = paste(pmin(from, to), pmax(from, to), sep = "_")) %>%
  group_by(pair) %>%
  mutate(
    has_stdm  = any(source == "stdm",  na.rm = TRUE),
    has_com = any(source == "com", na.rm = TRUE),
    is_double_pair = has_stdm & has_com
  ) %>%
  ungroup()

# Count how many unique pairs are doubles
n_doubles <- df_with_flags %>%
  filter(is_double_pair) %>%
  distinct(pair) %>%
  nrow()

n_doubles







# Step 4: Rebuild graph from combined edge list
g_com_12_merged <- graph_from_data_frame(df_collapsed, directed = FALSE)







# Step 7: Set uniform edge width
E(g_com_12_merged)$width <- 2

# Step 8: Style vertices
V(g_com_12_merged)$size <- 7
V(g_com_12_merged)$color <- "grey"
V(g_com_12_merged)[4]$color <- "green"  # Highlight node 14

# Step 9: Style edges (ignore KB+com_11)

E(g_com_12_merged)$color <- ifelse(E(g_com_12_merged)$source == "double", "green",
                                   ifelse(E(g_com_12_merged)$source == "stdm", "blue", "red"))

plot(
  g_com_12_merged,
  edge.color = E(g_com_12_merged)$color,
  edge.width = 2
)


# Step 10: Plot
pdf("/Users/dennisnientimp/Desktop/Data_Dennis/Stedum_data/merged network/g_com_12_merged_engl.pdf", width = 10, height = 8)# width and height are in inches

set.seed(1)
plot(g_com_12_merged,
     vertex.color = V(g_com_12_merged)$color,
     vertex.size = V(g_com_12_merged)$size,
     edge.color = E(g_com_12_merged)$color,
     edge.width = E(g_com_12_merged)$width)

# Step 11: Add legend (no magenta)
legend("topright", 
       legend = c("CEI", "Community Sample", "Overlap"),
       col = c("blue", "red", "green"),
       lty = 1, 
       lwd = 4,
       bty = "n",
       cex = 0.8)
dev.off()






# centrality --------------------------------------------------------------

if ("4" %in% V(g_com_12_merged)$name) {
  # Get the vertex ID
  v_id <- which(V(g_com_12_merged)$name == "4")
  
  # Degree Centrality
  deg <- degree(g_com_12_merged, v = v_id)
  
  # Closeness Centrality
  clo <- closeness(g_com_12_merged, v = v_id, normalized = TRUE)
  
  # Betweenness Centrality
  bet <- betweenness(g_com_12_merged, v = v_id, normalized = TRUE)
  
  # Eigenvector Centrality
  eig <- eigen_centrality(g_com_12_merged)$vector[v_id]
  
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
#   Degree: 16 
# Closeness: 0.3259259 
# Betweenness: 0.1007727 
# Eigenvector: 0.09592368 

# Mark nodes 2 steps away -------------------------------------------------



#lets look for the nodes exactly two steps away
which(distances(g_com_12_merged)["4", ] == 2)

library(igraph)

# Create color vector for all vertices
v_colors <- rep("gray", vcount(g_com_12_merged))  # default color

# Get vertex names exactly 2 steps away from node "4"
two_step_nodes <- names(which(distances(g_com_12_merged)["4", ] == 2))

# Set yellow for two-step nodes
v_colors[V(g_com_12_merged)$name %in% two_step_nodes] <- "yellow"

# Set green for node "4"
v_colors[V(g_com_12_merged)$name == "4"] <- "green"

# Plot with custom colors
set.seed(1)
plot(g_com_12_merged, vertex.color = v_colors, vertex.size = 10)

legend("topright", 
       legend = c("Duuzaam Stedum", "Steekproef"),
       col = c("blue", "red"),
       lty = 1, 
       lwd = 4,
       bty = "n",
       cex = 0.8)






library(igraph)

# Step 1: Get all nodes at distance exactly 2
dist_2_nodes <- names(which(distances(g_com_12_merged)["4", ] == 2))

# Step 2: Get direct neighbors (distance 1) using edge connections
direct_neighbors <- neighbors(g_com_12_merged, "4", mode = "all")$name

# Step 3: Filter out any node that is directly connected
pure_dist_2_nodes <- setdiff(dist_2_nodes, direct_neighbors)

# Step 4: Set up coloring
v_colors <- rep("lightgray", vcount(g_com_12_merged))
v_colors[V(g_com_12_merged)$name %in% pure_dist_2_nodes] <- "yellow"

# Set green for node "4"
v_colors[V(g_com_12_merged)$name == "4"] <- "green"

# Step 5: Plot
set.seed(1)
plot(g_com_12_merged, vertex.color = v_colors, vertex.size = 10, main= "clubs met alleen indirecte links")
legend("topright", 
       legend = c("Duuzaam Stedum", "Steekproef"),
       col = c("blue", "red"),
       lty = 1, 
       lwd = 4,
       bty = "n",
       cex = 0.8)




#most central clubs

deg_cent <- degree(g_com_12_merged)
top_deg <- sort(deg_cent, decreasing = TRUE)[1:10]
# 3  4 11 12 17  6 18 26 21 29 
# 41 25 25 24 19 14 12 12 11 11 

eig_cent <- eigen_centrality(g_com_12_merged)$vector
top_eig <- sort(eig_cent, decreasing = TRUE)[1:10]


#centrality measures
if ("14" %in% V(g_com_11_merged)$name) {
  # Get the vertex ID
  v_id <- which(V(g_com_11_merged)$name == "14")
  
  # Degree Centrality
  deg <- degree(g_com_11_merged, v = v_id)
  
  # Closeness Centrality
  clo <- closeness(g_com_11_merged, v = v_id, normalized = TRUE)
  
  # Betweenness Centrality
  bet <- betweenness(g_com_11_merged, v = v_id, normalized = TRUE)
  
  # Eigenvector Centrality
  eig <- eigen_centrality(g_com_11_merged)$vector[v_id]
  
  # Print results
  cat("Centrality measures for node '14':\n")
  cat("Degree:", deg, "\n")
  cat("Closeness:", clo, "\n")
  cat("Betweenness:", bet, "\n")
  cat("Eigenvector:", eig, "\n")
} else {
  cat("Node '14' not found in the graph.\n")
}


