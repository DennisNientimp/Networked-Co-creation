# Merging initiatives and sampled network ---------------------------------

g_com_11_club<- readRDS("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/g_com_11_club.rds")


g_kB <- readRDS("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/perc_data/g_KB_numbered_anonymous.rds")

#new script
library(igraph)


# Step 1: Tag edge sources
E(g_KB)$source <- "KB"
E(g_com_11_club)$source <- "com_11"


# Find the vertex with name "CEI_11" and rename it to "14"
V(g_com_11_club)$name[V(g_com_11_club)$name == "CEI_11"] <- "14"
V(g_com_11_club)$name <- gsub(" ", "", V(g_com_11_club)$name)
# Extract numeric part from each name and assign it back
V(g_com_11_club)$name <- gsub("[^0-9]", "", V(g_com_11_club)$name)


# Step 2: Extract edge lists with sources
df_KB <- as_data_frame(g_KB, what = "edges")
df_com <- as_data_frame(g_com_11_club, what = "edges")

# Get common columns
common_cols <- intersect(names(df_KB), names(df_com))

# Subset both data frames to common columns
df_KB_aligned <- df_KB[, common_cols]
df_com_aligned <- df_com[, common_cols]

# Step 3: Combine edge lists
df_combined <- rbind(df_KB_aligned , df_com_aligned)

# Step 4: Rebuild graph from combined edge list
g_com_11_merged <- graph_from_data_frame(df_combined, directed = FALSE)

# Step 5: Optional simplification
g_com_11_merged <- simplify(g_com_11_merged, remove.multiple = FALSE, remove.loops = FALSE)

# Step 7: Set uniform edge width
E(g_com_11_merged)$width <- 2

# Step 8: Style vertices
V(g_com_11_merged)$size <- 7
V(g_com_11_merged)$color <- "grey"
V(g_com_11_merged)[14]$color <- "green"  # Highlight node 14

# Step 9: Style edges (ignore KB+com_11)
E(g_com_11_merged)$color <- ifelse(grepl("KB", E(g_com_11_merged)$source), "blue", "red")



# Step 10: Plot
plot(g_com_11_merged,
     vertex.color = V(g_com_11_merged)$color,
     vertex.size = V(g_com_11_merged)$size,
     edge.color = E(g_com_11_merged)$color,
     edge.width = E(g_com_11_merged)$width)

# Step 11: Add legend (no magenta)
legend("topright", 
       legend = c("PEP5", "Steekproef"),
       col = c("blue", "red"),
       lty = 1, 
       lwd = 4,
       bty = "n",
       cex = 0.8)

Merging initiatives and sampled network ---------------------------------
  
  g_com_11_club<- readRDS("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/g_com_11_club.rds")
plot (g_com_11_club)

V(g_com_11_club)$name[ V(g_com_11_club)$name == "CEI_11" ] <- "14"


#new script
library(igraph)


# Step 1: Tag edge sources
E(g_KB)$source <- "KB"
E(g_com_11_club)$source <- "com_11"


# Find the vertex with name "CEI_11" and rename it to "14"
V(g_com_11_club)$name[V(g_com_11_club)$name == "CEI_11"] <- "14"



V(g_com_11_club)$name <- gsub(" ", "", V(g_com_11_club)$name)
# Extract numeric part from each name and assign it back
V(g_com_11_club)$name <- gsub("[^0-9]", "", V(g_com_11_club)$name)

library(igraph)
# Step 2: Extract edge lists with sources
df_KB <- igraph::as_data_frame(g_KB, what = "edges")
df_com <- igraph::as_data_frame(g_com_11_club, what = "edges")

#comobine them

df_KB  <- igraph::as_data_frame(g_KB, what = "edges")  %>%
  mutate(from = as.character(from),
         to   = as.character(to),
         source = "KB")

df_com <- igraph::as_data_frame(g_com_11_club, what = "edges") %>%
  mutate(from = as.character(from),
         to   = as.character(to),
         source = "com")

df_combined <- bind_rows(df_KB, df_com)


#merge dublicates
library(dplyr)

df_collapsed <- df_combined %>%
  # Create an unordered vertex-pair ID so 14–30 and 30–14 are treated the same
  mutate(pair = paste(pmin(from, to), pmax(from, to), sep = "_")) %>%
  
  group_by(pair) %>%
  # If a pair has both KB and com, keep one row and mark as "double"
  mutate(
    has_KB  = any(source == "KB", na.rm = TRUE),
    has_com = any(source == "com", na.rm = TRUE),
    source  = ifelse(has_KB & has_com, "double", source)
  ) %>%
  # Keep only one row per pair if it's a "double", otherwise keep all
  filter(!(source == "double" & duplicated(pair))) %>%
  ungroup() %>%
  select(-pair, -has_KB, -has_com)

df_with_flags <- df_combined %>%
  mutate(pair = paste(pmin(from, to), pmax(from, to), sep = "_")) %>%
  group_by(pair) %>%
  mutate(
    has_KB  = any(source == "KB",  na.rm = TRUE),
    has_com = any(source == "com", na.rm = TRUE),
    is_double_pair = has_KB & has_com
  ) %>%
  ungroup()

# Count how many unique pairs are doubles
n_doubles <- df_with_flags %>%
  filter(is_double_pair) %>%
  distinct(pair) %>%
  nrow()

n_doubles







# Step 4: Rebuild graph from combined edge list
g_com_11_merged <- graph_from_data_frame(df_collapsed, directed = FALSE)







# Step 7: Set uniform edge width
E(g_com_11_merged)$width <- 2

# Step 8: Style vertices
V(g_com_11_merged)$size <- 7
V(g_com_11_merged)$color <- "grey"
V(g_com_11_merged)[14]$color <- "green"  # Highlight node 14

# Step 9: Style edges (ignore KB+com_11)

E(g_com_11_merged)$color <- ifelse(E(g_com_11_merged)$source == "double", "green",
                                   ifelse(E(g_com_11_merged)$source == "KB", "blue", "red"))

plot(
  g_com_11_merged,
  edge.color = E(g_com_11_merged)$color,
  edge.width = 2
)


# Step 10: Plot
pdf("my_plot.pdf", width = 10, height = 8)  
# width and height are in inches
set.seed(1)
plot(g_com_11_merged,
     vertex.color = V(g_com_11_merged)$color,
     vertex.size = V(g_com_11_merged)$size,
     edge.color = E(g_com_11_merged)$color,
     edge.width = E(g_com_11_merged)$width)

# Step 11: Add legend (no magenta)
legend("topright", 
       legend = c("PEP5", "Steekproef", "allebei"),
       col = c("blue", "red", "green"),
       lty = 1, 
       lwd = 4,
       bty = "n",
       cex = 0.8)
dev.off()
saveRDS(g_com_11_merged, "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/g_com_11_merged.rds")

g_com_11_merged <-readRDS("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/g_com_11_merged.rds")


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

# Centrality measures for node '14':
#   Degree: 24 
# Closeness: 0.29375 
# Betweenness: 0.4898698 
# Eigenvector: 0.01010448 