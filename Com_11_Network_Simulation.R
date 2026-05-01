#empitical data Kloosterburen 11
rm(list = ls())

###INLEZEN VAN DE DATA VANUIT SPSS###


library(dplyr)

library(network)

library(foreign)
install.packages("xlsx")
library(xlsx)

install.packages("labelled")

library(labelled)

library(haven)
workfile <- read_sav('/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/KB_clean_anonymized.sav')
attach(workfile)

View(workfile)





#PLUG IN THE VALUES FOR EACH PERSON 
##### MATRIX B Person (X) to Event (Y) Matrix/dataframe (it is not really a matrix yet it is th items of the 5 clubs participants ca name on x and persons on y)

person_item_com_11 <- as.matrix(cbind(workfile$club_1, 
                                     workfile$club_2, 
                                     workfile$club_3, 
                                     workfile$club_4,
                                     workfile$club_5)) #making a matrix in the rigth format


#person_item_com_9 <- with(workfile, cbind(workfile$Club1_code_K, 
#workfile$Club2_code_K, 
#workfile$Club3_code, 
#workfile$Club4_code,
#workfile$Club5_code,
#workfile$Club6_code, 
#workfile$Club7_code)) #making a data frame


head(person_item_com_11) ### EERSTE 6 RIJEN VAN LIDMAATSCHAPSDATAMATRIX

n_clubs <- 51 ## AANTAL CLUBS INCLUSIEF HET TEAM

n <- 100

###B: EVENT (X) to PERSON (Y) MATRIX###
person_event_com_11 <- matrix(data = 0, nrow=n, ncol=n_clubs) #empty matrix with right amount of rows and columns and persons on x axis while clubs on y axis
View(person_event_com_11)

rownames(person_event_com_11) <- workfile$id #match the row names to the actual respondent numbers> necessary for later operations and to differentiate form clubs
#dont be confused some respondent names are up to 400 and some start wh an r
colnames(person_event_com_11) <- 1:n_clubs

#PLUG IN THE 1s  FOR EACH PERSON  EVENT CONNECTION
# I want to create and adjacency matrix. The respondents of (individuals are plotted on the y axis in the object person_event_com_2 and the associations they have links/ties with are on the x axis. Now I want to take the object person_item_com_2 which has respondents on the x axis and items on the y axis. The items asked respondents for their membership/ affiliations with the different affiliations that are denoted by the number of the club as a response to each of the 5 items. The numbers the respondents mentioned can be matched with the numbers serving as association IDs in the object person_event_com_2.  for every association and respondent that share a connection in person_item_com_2 I want to insert a 1 in the matching row/column in the object person_event_com_2.



# Convert character entries to numeric, replacing empty strings with NA
person_item_com_11 <- apply(person_item_com_11, 2, function(x) {
  x <- trimws(x)  # Remove leading/trailing spaces
  x[x == ""] <- NA  # Convert empty strings to NA
  as.numeric(x)  # Convert to numeric
})

# Now rerun your loop with a safer indexing condition
for (i in 1:n) {  
  for (j in 1:5) {  
    assoc_id <- person_item_com_11[i, j]  # Get the association ID from cleaned data
    if (!is.na(assoc_id) && assoc_id <= ncol(person_event_com_11)) {  
      person_event_com_11[i, assoc_id] <- 1  
    }
  }      
}



person_event_com_11 #

#MAKING THE FIRST NETWORK

library(igraph)

g_com_11 <- graph_from_incidence_matrix(incidence = person_event_com_11, mode = "all")
print_all(g_com_11)  


#checking name/label
vertex_attr(g_com_11)
#trim spaces
V(g_com_11)$name <- trimws(V(g_com_11)$name)

type_node <- vertex_attr(g_com_11, "type")
table(type_node) ##clearly: type_node
# type_node
# FALSE  TRUE 
# 100    51 
inwoner_vertices <- which(type_node =="FALSE")#save all vertices which are inwoners
length(inwoner_vertices) #worked


#max made mistake in numbering the clubs hence this code is needed

true_indices <- which(V(g_com_11)$type == TRUE)
V(g_com_11)$name[true_indices] <- as.character(seq_along(true_indices) - 1)



#rename CEI node
V(g_com_11)$name[V(g_com_11)$name == "14" & V(g_com_11)$type == TRUE] <- "CEI_11"


#check who intiators are and whether they have ties
filtered_df <- workfile %>% 
  filter(Initiator == "1") %>%
  select(Initiator, id)

# Initiator  id
# 1         1 103
# 2         1 104
# 3         1 105
# 4         1 106
# 5         1 107
# 6         1 108
# 7         1 109





#make respondents red and small
V(g_com_11 )$color[type_node == FALSE] <- rgb(red = 1, green = 0, blue = 0, alpha = 0.5)
V(g_com_11 )$size[type_node == FALSE] <- 3
V(g_com_11)$label.cex[V(g_com_11)$type == FALSE] <- 0.5

#make clubs blue and big ass nodes
V(g_com_11 )$color[type_node == TRUE] <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
V(g_com_11 )$size[type_node == TRUE] <- 10

#set edge color
E(g_com_11)$color <- rgb(0, 1, 0, 1)

#define vertex shape
V(g_com_11)$shape <- "circle"

layout <- layout_with_fr(g_com_11, grid = "nogrid")


#highlight CEI  
V(g_com_11)$color[vertex_attr(graph = g_com_11)$name == "CEI_11"] <- rgb(0.5,0,0.5,0.75)
# Assign "square" shape to "CEI_5"
V(g_com_11)$shape[V(g_com_11)$name == "CEI_11"] <- "square"

#also highlight intiaitve takers 

V(g_com_11)$color[vertex_attr(graph = g_com_11)$name %in% c("103", "104", "105", "106", "107", "108", "109")] <- rgb(1,1,0,0.75)



set.seed(1)
plot(g_com_11, layout = layout_with_fr)

#lets somehow save or graph object
saveRDS(g_com_11, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/g_com_11.rds')
g_com_11_loaded <- readRDS('/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/g_com_11.rds')
plot(g_com_11_loaded)

g_com_11 <- g_com_11_loaded


# One mode projections/ clustering ----------------------------------------


# First, we need to get to the one-mode person-to-person matrix
onemode <- bipartite_projection(g_com_11)
g_com_11_ptp <- onemode$proj1  # Select the first graph, which is the one-mode network of ptp
length(g_com_11_ptp)
# 100

# Let's grab the matrix
g_com_11_mat <- as_adjacency_matrix(graph = g_com_11_ptp, attr = "weight", sparse = F)
head(g_com_11_mat)

# Now let's plot it!
V(g_com_11_ptp)$label.color <- rgb(0, 0, .5, .8)

V(g_com_11_ptp)$label.cex <- 1 
V(g_com_11_ptp)$size <- 8
V(g_com_11_ptp)$color <- rgb(0, 0, 0, .3)
V(g_com_11_ptp)$frame.color <- V(g_com_11_ptp)$color 

# Edge transparency based on individuals connected to it
egalpha <- log1p(E(g_com_11_ptp)$weight) / max(log1p(E(g_com_11_ptp)$weight))
E(g_com_11_ptp)$color <- rgb(.25, .75, 0, egalpha)
E(g_com_11_ptp)$width <- egalpha * 4
set.seed(1)
plot(g_com_11_ptp)
dev.off()

## Let's highlight initiative takers


# Change only the shape of the specified nodes
# Nodes to be squared
nodes_to_square <- c("103", "104", "105", "106", "107", "108", "109")

# Apply shape change to those nodes
V(g_com_11_ptp)[V(g_com_11_ptp)$name %in% nodes_to_square]$shape <- "square"
V(g_com_11_ptp)[V(g_com_11_ptp)$name %in% nodes_to_square]$frame.color <- "red"

set.seed(1)
plot(g_com_11_ptp, layout = layout_with_fr)

# Cluster modularity score
cluster_fast <- cluster_fast_greedy(graph = g_com_11_ptp)
options(max.print = 100000)  # Adjust the number as needed
print(cluster_fast)
set.seed(1)
plot(cluster_fast, g_com_11_ptp, main = "Modularity optimization algorithm")

# Let's remove isolates and also grab the matrix
degree0 <- which(degree(g_com_11_ptp) == 0)
g_com_11_ptp_no_iso <- delete_vertices(g_com_11_ptp, degree0)

g_com_11_ptp_no_iso_mat <- as_adjacency_matrix(g_com_11_ptp_no_iso)

# Now let's plot it!
V(g_com_11_ptp_no_iso)$label.color <- rgb(0, 0, .5, .8)

V(g_com_11_ptp_no_iso)$label.cex <- 1 
V(g_com_11_ptp_no_iso)$size <- 8
V(g_com_11_ptp_no_iso)$color <- rgb(0, 0, 0, .3)

# Edge transparency based on individuals connected to it

egalpha <- log1p(E(g_com_11_ptp_no_iso)$weight) / max(log1p(E(g_com_11_ptp_no_iso)$weight))
E(g_com_11_ptp_no_iso)$color <- rgb(.25, .75, 0, egalpha)
E(g_com_11_ptp_no_iso)$width <- egalpha * 4

# Cluster modularity score
cluster_fast_11 <- cluster_fast_greedy(graph = g_com_11_ptp_no_iso)
options(max.print = 100000)  # Adjust the number as needed
print(cluster_fast)

## Let's highlight initiative takers
# Also highlight initiative takers and identify their labels

nodes_to_square <- c("103", "104", "105", "106", "107", "108", "109")

# Apply shape change to those nodes
V(g_com_11_ptp_no_iso)[V(g_com_11_ptp_no_iso)$name %in% nodes_to_square]$shape <- "square"
V(g_com_11_ptp_no_iso)[V(g_com_11_ptp_no_iso)$name %in% nodes_to_square]$frame.color <- "red"
set.seed(1)
plot(cluster_fast_11, g_com_11_ptp_no_iso, main ="Modularity optimization algorithm")






# club_club_network -------------------------------------------------------

onemode <- bipartite_projection(g_com_11)
g_com_11_club <- onemode$proj2  

set.seed(3)
plot(g_com_11_club)

saveRDS(g_com_11_club, "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/g_com_11_club.rds")
g_com_11_club<- readRDS("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/g_com_11_club.rds")

# diffusion ---------------------------------------------------------------

library(netdiffuseR)



#just for now make some cheap willingness to participate variable
library(psych)
df_willingness <- data.frame(
  join_meeting = workfile$join_meeting,
  join_finan = workfile$join_finan,
  join_volun = workfile$join_volun
)



library(dplyr)
library(dplyr)



alpha(df_willingness)
# Reliability analysis   
# Call: alpha(x = df_willingness)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
# 0.83      0.83    0.77      0.62 4.9 0.029 0.65 0.61     0.61
# 
# 95% confidence boundaries 
# lower alpha upper
# Feldt     0.76  0.83  0.88
# Duhachek  0.77  0.83  0.88
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
# join_meeting      0.71      0.71    0.55      0.55 2.5    0.058    NA  0.55
# join_finan        0.76      0.76    0.61      0.61 3.2    0.048    NA  0.61
# join_volun        0.81      0.82    0.69      0.69 4.5    0.037    NA  0.69
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean   sd
# join_meeting 93  0.90  0.89  0.82   0.74 0.85 0.77
# join_finan   93  0.85  0.87  0.77   0.70 0.61 0.63
# join_volun   93  0.84  0.84  0.69   0.64 0.49 0.72
# 
# Non missing response frequency for each item
# 0    1    2 miss
# join_meeting 0.38 0.40 0.23 0.07
# join_finan   0.46 0.46 0.08 0.07
# join_volun   0.63 0.24 0.13 0.07


workfile$willing_to_part <- rowMeans(df_willingness, na.rm = TRUE)




# CALIBRATION: Adding realism to Thresholds 
library(haven)
df_survey <- read.csv2('/Users/dennisnientimp/Desktop/Diffusion Simulations/imputed_data.csv')
View(df_survey)


# Create a data frame with id and willingness to participate
threshold_com_11 <- workfile[, c("id", "willing_to_part")]
View(threshold_com_11)

# -------------------------------
# THRESHOLD_2 calibration
# -------------------------------

lookup_table_2 <- data.frame(
  threshold   = c(0, 0.25, 0.333333, 0.366667, 1, 1.25, 1.333333, 1.5, 1.666667, 1.75, 2),
  numerator   = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0),  
  denominator = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1),
  multiplier  = c(6, 5.5, 5, 4.5, 4, 3.5, 3, 2.5, 2, 1.5, 0)
)


adjust_thresholds_lookup <- function(df, lookup_table, tolerance = 1e-5) {
  # Loop through each row in the lookup table
  for(i in seq_len(nrow(lookup_table))) {
    thr <- lookup_table$threshold[i]
    # Compute the output value using a fraction times the multiplier
    new_value <- (lookup_table$numerator[i] / lookup_table$denominator[i]) * lookup_table$multiplier[i]
    
    # For all rows where the df value is within tolerance of the threshold,
    # replace it with the computed new_value
    df$willing_to_part[ abs(df$willing_to_part - thr) < tolerance ] <- new_value
  }
  return(df)
}

threshold_com_11_2 <- adjust_thresholds_lookup(threshold_com_11, lookup_table_2, tolerance = 0.11)

# -------------------------------
# THRESHOLD_5 calibration
# -------------------------------

lookup_table_5 <- lookup_table_2
lookup_table_5$numerator <- 5

threshold_com_11_5 <- adjust_thresholds_lookup(threshold_com_11, lookup_table_5, tolerance = 0.11)

# -------------------------------
# THRESHOLD_7 calibration
# -------------------------------

lookup_table_7 <- lookup_table_2
lookup_table_7$numerator <- 7

threshold_com_11_7 <- adjust_thresholds_lookup(threshold_com_11, lookup_table_7, tolerance = 0.11)

# -------------------------------
# Combining Survey and Network Data
# -------------------------------

make_thresh_vector <- function(threshold_df, net_mat, casenr_col, thresh_col, fill_method = "mean", tolerance = 1e-5) {
  threshold_df[[casenr_col]] <- trimws(as.character(threshold_df[[casenr_col]]))
  node_ids <- as.character(rownames(net_mat))
  match_idx <- match(node_ids, threshold_df[[casenr_col]])
  matched_values <- threshold_df[[thresh_col]][match_idx]
  threshold_vector <- rep(NA, length(node_ids))
  threshold_vector[!is.na(match_idx)] <- matched_values[!is.na(match_idx)]
  if (fill_method == "mean") {
    threshold_vector[is.na(threshold_vector)] <- mean(matched_values, na.rm = TRUE)
  } else if (fill_method == "zero") {
    threshold_vector[is.na(threshold_vector)] <- 0
  }
  return(threshold_vector)
}

# Create threshold vectors for each threshold condition using com_9 data and network g_com_9_mat
com_11_thresh_v_2 <- make_thresh_vector(threshold_com_11_2, g_com_11_mat, "id", "willing_to_part", "mean")
com_11_thresh_v_5 <- make_thresh_vector(threshold_com_11_5, g_com_11_mat, "id", "willing_to_part", "mean")
com_11_thresh_v_7 <- make_thresh_vector(threshold_com_11_7, g_com_11_mat, "id", "willing_to_part", "mean")


saveRDS (com_11_thresh_v_2, "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_v_2.rds")
saveRDS (com_11_thresh_v_5 , "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_v_5 .rds")
saveRDS (com_11_thresh_v_7 , "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_v_7 .rds")


com_11_thresh_v_2 <- read_rds( "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_v_2.rds")
com_11_thresh_v_5 <- read_rds( "/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_v_5 .rds")
com_11_thresh_v_7 <- read_rds("/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_v_7 .rds")

# -------------------------------
# Setting Initiators to 0 (They Always Adopt)
# -------------------------------

V(g_com_11_ptp)$name

vertex_names_to_replace <- c("103", "104", "105", "106", "107", "108", "109")

indices_to_replace_com_11 <- match(vertex_names_to_replace, V(g_com_11_ptp)$name)
print(indices_to_replace_com_11)
# 94  95  96  97  98  99 100
com_11_thresh_v_2[indices_to_replace_com_11] <- 0
com_11_thresh_v_5[indices_to_replace_com_11] <- 0
com_11_thresh_v_7[indices_to_replace_com_11] <- 0


# Seed --------------------------------------------------------------------

# We change our approach – let's select 5% of the nodes in the network
0.05 * 100
# 5 We select 4 nodes

# 5% most central nodes for central seeding
# Compute centrality
eigen <- eigen_centrality(g_com_11_ptp)
# Extract the centrality vector from the eigen list
centrality_values <- eigen$vector

eigen_top_indices <- order(centrality_values, decreasing = TRUE)

# View result
print(eigen_top_indices)
# 2 68  35  36  49  34  33  10  84   6  95  96 100  17  50  29  66
# Let's select the 5% most central
seeds_central_com_11 <- c(2, 68, 35, 36, 49) # Always check whether the initiators are among these! Best practice: put the ptp graph next to it and look at it!

# Then I like to check whether these nodes really appear to be central in the network
matching_names <- V(g_com_11_ptp)$name[seeds_central_com_11]

# Print the results
print(matching_names)

# # Subcentral -------------------------------------------------------------

# 1. Detect communities using the fast greedy algorithm
communities <- cluster_fast_greedy(g_com_11_ptp)

# 2. Compute degree centrality for all nodes
degree_centrality <- degree(g_com_11_ptp)

# 3. Initialize a data frame to store the results
community_data <- data.frame(
  Community = integer(),
  Most_Central_Node_Indexes = character(),
  Total_Nodes = integer(),
  stringsAsFactors = FALSE
)

# 4. Iterate through communities and retrieve numeric node indices
for (i in 1:length(communities)) {
  community_nodes <- communities[[i]]
  
  if (!is.numeric(community_nodes)) {
    community_indices <- match(community_nodes, V(g_com_11_ptp)$name)
  } else {
    community_indices <- community_nodes
  }
  
  total_nodes <- length(community_indices)
  
  sorted_indices <- community_indices[order(-degree_centrality[community_indices])]
  
  top_indices <- head(sorted_indices, 5)
  
  community_data <- rbind(
    community_data,
    data.frame(
      Community = i,
      Most_Central_Node_Indexes = paste(top_indices, collapse = ", "),
      Total_Nodes = total_nodes,
      stringsAsFactors = FALSE
    )
  )
}

# Print the table with the community information
print(community_data)
# Community Most_Central_Node_Indexes Total_Nodes
# Community Most_Central_Node_Indexes Total_Nodes
# 1          1        66, 17, 11, 21, 37          11
# 2          2       95, 96, 100, 93, 97          13
# 3          3         29, 60, 73, 5, 69          10
# 4          4         49, 84, 6, 20, 28          13
# 5          5        68, 35, 36, 33, 34          12
# 6          6                         1           1


# Now let's target the most subcentral nodes in a seeding strategy 
seeds_subcentral_com_11 <- c(66, 93,29, 49, 68) 

matching_names <- V(g_com_11_ptp)$name[seeds_subcentral_com_11]

# Print the results
print(matching_names)

# # THRESHOLD 2 for com_9 -------------------------------------------------

# define the functions
stat_func <- function(x) { 
  cumulative_adopt_count(x)["prop", ]
}

com_11_thresh_2_rand_uw_rw.2 <- rdiffnet_multiple(
  statistic = stat_func,
  R = 5000,
  ncpus = 8,
  t = 100,
  seed.nodes = "random",
  seed.p.adopt = 0.05,
  seed.graph = g_com_11_mat,
  rewire = TRUE,
  rgraph.args = list(p = .2),
  threshold.dist = com_11_thresh_v_2, 
  exposure.args = list(normalized = FALSE), 
  stop.no.diff = FALSE,
  name = "Diffusion",
  behavior = "CEI participation"
)

rownames(g_com_11_mat) <- make.unique(rownames(g_com_11_mat))

# 5% most central
com_11_thresh_2_centr_uw_rw.2 <- rdiffnet_multiple(
  statistic = stat_func,
  R = 5000, 
  ncpus = 8,
  t = 100, 
  seed.nodes = seeds_central_com_11,
  seed.graph = g_com_11_mat,
  rewire = TRUE,
  rgraph.args = list(p = .2),
  threshold.dist = com_11_thresh_v_2, 
  exposure.args = list(normalized = FALSE), 
  stop.no.diff = FALSE, 
  name = "Diffusion", 
  behavior = "CEI participation"
)

# 5% most subcentral
com_11_thresh_2_subc_uw_rw.2 <- rdiffnet_multiple(
  statistic = stat_func,
  R = 5000, 
  ncpus = 8,
  t = 100, 
  seed.nodes = seeds_subcentral_com_11,
  seed.graph = g_com_11_mat,
  rewire = TRUE,
  rgraph.args = list(p = .2),
  threshold.dist = com_11_thresh_v_2, 
  exposure.args = list(normalized = FALSE), 
  stop.no.diff = FALSE, 
  name = "Diffusion", 
  behavior = "CEI participation"
)

# Save the simulations for threshold 2
saveRDS(com_11_thresh_2_rand_uw_rw.2, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_2_rand_uw_rw.2.rds')
saveRDS(com_11_thresh_2_centr_uw_rw.2, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_2_centr_uw_rw.2.rds')
saveRDS(com_11_thresh_2_subc_uw_rw.2, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_2_subc_uw_rw.2.rds')

plot_simulation_outcomes <- function(random_data, intervention_data, subcentral_data, plot_title) {
  par(mfrow = c(3, 1))  
  
  boxplot(t(random_data), xlab = "Time", 
          ylab = "Proportion of Adopters",  
          main = "Random Seed Model", boxwex = 0.5, ylim = c(0, .8))
  
  boxplot(t(intervention_data), xlab = "Time", 
          ylab = "Proportion of Adopters",  
          main = "5% Most Central", boxwex = 0.5, ylim = c(0, .8))
  
  boxplot(t(subcentral_data), xlab = "Time", 
          ylab = "Proportion of Adopters",  
          main = "5% Most Subcentral", boxwex = 0.5, ylim = c(0, .8))
  
  mtext(plot_title, outer = TRUE, cex = 1.2, font = 2, line = -1)
}

dev.off()

output_path <- '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_2_uw_rw.2.pdf'
pdf(output_path, width = 8, height = 6)
plot_simulation_outcomes(com_11_thresh_2_rand_uw_rw.2, 
                         com_11_thresh_2_centr_uw_rw.2, 
                         com_11_thresh_2_subc_uw_rw.2, 
                         "Com_11_Threshold = 2 / Unweighted Network")
dev.off()






# THRESHOLD 5 for com_9 -------------------------------------------------

com_11_thresh_5_rand_uw_rw.2 <- rdiffnet_multiple(
  statistic = stat_func,
  R = 5000,
  ncpus = 8,
  t = 100,
  seed.nodes = "random",
  seed.p.adopt = 0.05,
  seed.graph = g_com_11_mat,
  rewire = TRUE,
  rgraph.args = list(p = .2),
  threshold.dist = com_11_thresh_v_5,
  exposure.args = list(normalized = FALSE),
  stop.no.diff = FALSE,
  name = "Diffusion",
  behavior = "CEI participation"
)

# 5% most central
com_11_thresh_5_centr_uw_rw.2 <- rdiffnet_multiple(
  statistic = stat_func,
  R = 5000,
  ncpus = 8,
  t = 100,
  seed.nodes = seeds_central_com_11,
  seed.graph = g_com_11_mat,
  rewire = TRUE,
  rgraph.args = list(p = .2),
  threshold.dist = com_11_thresh_v_5,
  exposure.args = list(normalized = FALSE),
  stop.no.diff = FALSE,
  name = "Diffusion",
  behavior = "CEI participation"
)

# 5% most subcentral
com_11_thresh_5_subc_uw_rw.2 <- rdiffnet_multiple(
  statistic = stat_func,
  R = 5000,
  ncpus = 8,
  t = 100,
  seed.nodes = seeds_subcentral_com_11,
  seed.graph = g_com_11_mat,
  rewire = TRUE,
  rgraph.args = list(p = .2),
  threshold.dist = com_11_thresh_v_5,
  exposure.args = list(normalized = FALSE),
  stop.no.diff = FALSE,
  name = "Diffusion",
  behavior = "CEI participation"
)

# Save the simulations for threshold 5
saveRDS(com_11_thresh_5_rand_uw_rw.2, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_5_rand_uw_rw.2.rds')
saveRDS(com_11_thresh_5_centr_uw_rw.2, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_5_centr_uw_rw.2.rds')
saveRDS(com_11_thresh_5_subc_uw_rw.2, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_5_subc_uw_rw.2.rds')

output_path <- '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_5_uw_rw.2.pdf'
pdf(output_path, width = 8, height = 6)
plot_simulation_outcomes(com_11_thresh_5_rand_uw_rw.2, 
                         com_11_thresh_5_centr_uw_rw.2, 
                         com_11_thresh_5_subc_uw_rw.2, 
                         "Com_11_Threshold = 5 / Unweighted Network")
dev.off()

# THRESHOLD 7 for com_9 ---------------------------------------------------

com_11_thresh_7_rand_uw_rw.2 <- rdiffnet_multiple(
  statistic = stat_func,
  R = 5000,
  ncpus = 8,
  t = 100,
  seed.nodes = "random",
  seed.p.adopt = 0.05,
  seed.graph = g_com_11_mat,
  rewire = TRUE,
  rgraph.args = list(p = .2),
  threshold.dist = com_11_thresh_v_7, 
  exposure.args = list(normalized = FALSE), 
  stop.no.diff = FALSE,
  name = "Diffusion",
  behavior = "CEI participation"
)

# 5% most central
com_11_thresh_7_centr_uw_rw.2 <- rdiffnet_multiple(
  statistic = stat_func,
  R = 5000,
  ncpus = 8,
  t = 100,
  seed.nodes = seeds_central_com_11,
  seed.graph = g_com_11_mat,
  rewire = TRUE,
  rgraph.args = list(p = .2),
  threshold.dist = com_11_thresh_v_7, 
  exposure.args = list(normalized = FALSE), 
  stop.no.diff = FALSE,
  name = "Diffusion",
  behavior = "CEI participation"
)

# 5% most subcentral
com_11_thresh_7_subc_uw_rw.2 <- rdiffnet_multiple(
  statistic = stat_func,
  R = 5000,
  ncpus = 8,
  t = 100,
  seed.nodes = seeds_subcentral_com_11,
  seed.graph = g_com_11_mat,
  rewire = TRUE,
  rgraph.args = list(p = .2),
  threshold.dist = com_11_thresh_v_7, 
  exposure.args = list(normalized = FALSE), 
  stop.no.diff = FALSE,
  name = "Diffusion",
  behavior = "CEI participation"
)

# Save the simulations for threshold 7
saveRDS(com_11_thresh_7_rand_uw_rw.2, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_7_rand_uw_rw.2.rds')
saveRDS(com_11_thresh_7_centr_uw_rw.2, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_7_centr_uw_rw.2.rds')
saveRDS(com_11_thresh_7_subc_uw_rw.2, '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_7_subc_uw_rw.2.rds')

output_path <- '/Users/dennisnientimp/Desktop/Data_Dennis/Kloosterburen_data/empirical_data/com_11_thresh_7_uw_rw.2.pdf'
pdf(output_path, width = 8, height = 6)
plot_simulation_outcomes(com_11_thresh_7_rand_uw_rw.2, 
                         com_11_thresh_7_centr_uw_rw.2, 
                         com_11_thresh_7_subc_uw_rw.2, 
                         "Com_11_Threshold = 7 / Unweighted Network")
dev.off()

# plotting conditions ----------------------------------------------------
plot_threshold_2 <- function(thresh_random, thresh_central, thresh_subcentral, 
                             time_range = 1:100, 
                             colors = c("blue", "red", "green")) {
  # Compute summary statistics for random seeds
  avg_thresh_random <- rowMeans(thresh_random, na.rm = TRUE)
  sd_thresh_random <- apply(thresh_random, 1, sd, na.rm = TRUE)
  quantiles_thresh_random <- apply(thresh_random, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Compute summary statistics for central seeds
  avg_thresh_central <- rowMeans(thresh_central, na.rm = TRUE)
  sd_thresh_central <- apply(thresh_central, 1, sd, na.rm = TRUE)
  quantiles_thresh_central <- apply(thresh_central, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Compute summary statistics for subcentral seeds
  avg_thresh_subcentral <- rowMeans(thresh_subcentral, na.rm = TRUE)
  sd_thresh_subcentral <- apply(thresh_subcentral, 1, sd, na.rm = TRUE)
  quantiles_thresh_subcentral <- apply(thresh_subcentral, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Set up the base plot using random seeds
  plot(time_range, avg_thresh_random, type = "l", col = colors[1], lwd = 2, 
       xlab = "Time", ylab = "Proportion of Adopters", 
       main = "Threshold = 2 ", cex.main = 2, cex.lab = 1.5, ylim = c(0, 0.8))
  
  # Add polygons for the ±1 standard deviation bands
  polygon(c(time_range, rev(time_range)), 
          c(avg_thresh_random + sd_thresh_random, rev(avg_thresh_random - sd_thresh_random)),
          col = rgb(0, 0, 1, 0.2), border = NA)
  polygon(c(time_range, rev(time_range)), 
          c(avg_thresh_central + sd_thresh_central, rev(avg_thresh_central - sd_thresh_central)),
          col = rgb(1, 0, 0, 0.2), border = NA)
  polygon(c(time_range, rev(time_range)), 
          c(avg_thresh_subcentral + sd_thresh_subcentral, rev(avg_thresh_subcentral - sd_thresh_subcentral)),
          col = rgb(0, 1, 0, 0.2), border = NA)
  
  # Overlay dashed lines for the 5th and 95th quantiles
  lines(time_range, quantiles_thresh_random[1, ], col = colors[1], lty = 2)
  lines(time_range, quantiles_thresh_random[2, ], col = colors[1], lty = 2)
  
  lines(time_range, quantiles_thresh_central[1, ], col = colors[2], lty = 2)
  lines(time_range, quantiles_thresh_central[2, ], col = colors[2], lty = 2)
  
  lines(time_range, quantiles_thresh_subcentral[1, ], col = colors[3], lty = 2)
  lines(time_range, quantiles_thresh_subcentral[2, ], col = colors[3], lty = 2)
  
  # Overlay the average lines for central and subcentral seed scenarios
  lines(time_range, avg_thresh_central, col = colors[2], lwd = 2)
  lines(time_range, avg_thresh_subcentral, col = colors[3], lwd = 2)
  
  # Add a legend to the top right
  legend("topright", legend = c("Random Seeds", "Central Seeds", "Subcentral Seeds"), 
         col = colors, lwd = 2, cex = 1.5)
}


plot_threshold_5 <- function(thresh_random, thresh_central, thresh_subcentral, 
                             time_range = 1:100, 
                             colors = c("blue", "red", "green")) {
  # Compute summary statistics for random seeds
  avg_thresh_random <- rowMeans(thresh_random, na.rm = TRUE)
  sd_thresh_random <- apply(thresh_random, 1, sd, na.rm = TRUE)
  quantiles_thresh_random <- apply(thresh_random, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Compute summary statistics for central seeds
  avg_thresh_central <- rowMeans(thresh_central, na.rm = TRUE)
  sd_thresh_central <- apply(thresh_central, 1, sd, na.rm = TRUE)
  quantiles_thresh_central <- apply(thresh_central, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Compute summary statistics for subcentral seeds
  avg_thresh_subcentral <- rowMeans(thresh_subcentral, na.rm = TRUE)
  sd_thresh_subcentral <- apply(thresh_subcentral, 1, sd, na.rm = TRUE)
  quantiles_thresh_subcentral <- apply(thresh_subcentral, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Set up the base plot using random seeds
  plot(time_range, avg_thresh_random, type = "l", col = colors[1], lwd = 2, 
       xlab = "Time", ylab = "Proportion of Adopters", 
       main = "Threshold = 5", cex.main = 2, cex.lab = 1.5, ylim = c(0, 0.8))
  
  # Add polygons for the ±1 standard deviation bands
  polygon(c(time_range, rev(time_range)), 
          c(avg_thresh_random + sd_thresh_random, rev(avg_thresh_random - sd_thresh_random)),
          col = rgb(0, 0, 1, 0.2), border = NA)
  polygon(c(time_range, rev(time_range)), 
          c(avg_thresh_central + sd_thresh_central, rev(avg_thresh_central - sd_thresh_central)),
          col = rgb(1, 0, 0, 0.2), border = NA)
  polygon(c(time_range, rev(time_range)), 
          c(avg_thresh_subcentral + sd_thresh_subcentral, rev(avg_thresh_subcentral - sd_thresh_subcentral)),
          col = rgb(0, 1, 0, 0.2), border = NA)
  
  # Overlay dashed lines for the 5th and 95th quantiles
  lines(time_range, quantiles_thresh_random[1, ], col = colors[1], lty = 2)
  lines(time_range, quantiles_thresh_random[2, ], col = colors[1], lty = 2)
  
  lines(time_range, quantiles_thresh_central[1, ], col = colors[2], lty = 2)
  lines(time_range, quantiles_thresh_central[2, ], col = colors[2], lty = 2)
  
  lines(time_range, quantiles_thresh_subcentral[1, ], col = colors[3], lty = 2)
  lines(time_range, quantiles_thresh_subcentral[2, ], col = colors[3], lty = 2)
  
  # Overlay the average lines for central and subcentral seed scenarios
  lines(time_range, avg_thresh_central, col = colors[2], lwd = 2)
  lines(time_range, avg_thresh_subcentral, col = colors[3], lwd = 2)
  
  # Add a legend to the top right
  legend("topright", legend = c("Random Seeds", "Central Seeds", "Subcentral Seeds"), 
         col = colors, lwd = 2, cex = 1.5)
}


plot_threshold_7 <- function(thresh_random, thresh_central, thresh_subcentral, 
                             time_range = 1:100, 
                             colors = c("blue", "red", "green")) {
  # Compute summary statistics for random seeds
  avg_thresh_random <- rowMeans(thresh_random, na.rm = TRUE)
  sd_thresh_random <- apply(thresh_random, 1, sd, na.rm = TRUE)
  quantiles_thresh_random <- apply(thresh_random, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Compute summary statistics for central seeds
  avg_thresh_central <- rowMeans(thresh_central, na.rm = TRUE)
  sd_thresh_central <- apply(thresh_central, 1, sd, na.rm = TRUE)
  quantiles_thresh_central <- apply(thresh_central, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Compute summary statistics for subcentral seeds
  avg_thresh_subcentral <- rowMeans(thresh_subcentral, na.rm = TRUE)
  sd_thresh_subcentral <- apply(thresh_subcentral, 1, sd, na.rm = TRUE)
  quantiles_thresh_subcentral <- apply(thresh_subcentral, 1, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Set up the base plot using random seeds
  plot(time_range, avg_thresh_random, type = "l", col = colors[1], lwd = 2, 
       xlab = "Time", ylab = "Proportion of Adopters", 
       main = "Threshold = 7", cex.main = 2, cex.lab = 1.5, ylim = c(0, 0.8))
  
  # Add polygons for the ±1 standard deviation bands
  polygon(c(time_range, rev(time_range)), 
          c(avg_thresh_random + sd_thresh_random, rev(avg_thresh_random - sd_thresh_random)),
          col = rgb(0, 0, 1, 0.2), border = NA)
  polygon(c(time_range, rev(time_range)), 
          c(avg_thresh_central + sd_thresh_central, rev(avg_thresh_central - sd_thresh_central)),
          col = rgb(1, 0, 0, 0.2), border = NA)
  polygon(c(time_range, rev(time_range)), 
          c(avg_thresh_subcentral + sd_thresh_subcentral, rev(avg_thresh_subcentral - sd_thresh_subcentral)),
          col = rgb(0, 1, 0, 0.2), border = NA)
  
  # Overlay dashed lines for the 5th and 95th quantiles
  lines(time_range, quantiles_thresh_random[1, ], col = colors[1], lty = 2)
  lines(time_range, quantiles_thresh_random[2, ], col = colors[1], lty = 2)
  
  lines(time_range, quantiles_thresh_central[1, ], col = colors[2], lty = 2)
  lines(time_range, quantiles_thresh_central[2, ], col = colors[2], lty = 2)
  
  lines(time_range, quantiles_thresh_subcentral[1, ], col = colors[3], lty = 2)
  lines(time_range, quantiles_thresh_subcentral[2, ], col = colors[3], lty = 2)
  
  # Overlay the average lines for central and subcentral seed scenarios
  lines(time_range, avg_thresh_central, col = colors[2], lwd = 2)
  lines(time_range, avg_thresh_subcentral, col = colors[3], lwd = 2)
  
  # Add a legend to the top right
  legend("topright", legend = c("Random Seeds", "Central Seeds", "Subcentral Seeds"), 
         col = colors, lwd = 2, cex = 1.5)
}



par(mfrow = c(1, 3))

plot_threshold_2(get("com_11_thresh_2_rand_uw_rw.2"), 
                 get("com_11_thresh_2_centr_uw_rw.2"), 
                 get("com_11_thresh_2_subc_uw_rw.2"))
plot_threshold_5(get("com_11_thresh_5_rand_uw_rw.2"), 
                 get("com_11_thresh_5_centr_uw_rw.2"), 
                 get("com_11_thresh_5_subc_uw_rw.2"))
plot_threshold_7(get("com_11_thresh_7_rand_uw_rw.2"), 
                 get("com_11_thresh_7_centr_uw_rw.2"), 
                 get("com_11_thresh_7_subc_uw_rw.2"))



# Slices and GIF ----------------------------------------------------------

#slices for demonstration
library(netdiffuseR)

# Run a single diffusion simulation for the central condition
com_11_thresh_5_centr_single <- rdiffnet(
  seed.graph       = g_com_11_mat,
  seed.nodes       = seeds_central_com_11,
  threshold.dist   = com_11_thresh_v_7,
  t                = 10,
  rewire           = TRUE,
  rgraph.args      = list(p = .2),
  exposure.args    = list(normalized = FALSE),
  stop.no.diff     = FALSE,
  name             = "Diffusion",
  behavior         = "CEI participation"
)
rownames(g_com_11_mat) <- make.unique(rownames(g_com_11_mat))

# Quick check of the object
summary(com_11_thresh_2_centr_single)

# Plot the adoption curve over time
set.seed(1)
plot_diffnet(com_11_thresh_2_centr_single, slices = c(1,3,5,10))

install.packages("animation")   # only once
library(animation)
install.packages("animation")   # if not installed
library(animation)



library(netdiffuseR)
library(igraph)

# Helper: extract adjacency at time t from a diffnet object, regardless of storage
get_adj_at <- function(dn, t = 1) {
  # Try common slots: 'graphs' or 'graph'
  if ("graphs" %in% names(dn)) {
    G <- dn$graphs
  } else if ("graph" %in% names(dn)) {
    G <- dn$graph
  } else {
    stop("Could not find adjacency storage in diffnet object. Slots: ",
         paste(names(dn), collapse = ", "))
  }
  # If it's a 3D array (n x n x T)
  if (!is.null(dim(G)) && length(dim(G)) == 3) {
    return(as.matrix(G[,,t, drop = FALSE][,,1]))
  }
  # If it's a list of matrices/sparse matrices
  if (is.list(G)) {
    if (t > length(G)) stop("Requested t exceeds number of stored graphs.")
    return(as.matrix(G[[t]]))
  }
  stop("Unrecognized graph storage format (expected 3D array or list).")
}

# 1) Extract adjacency at time 1 and compute a fixed layout
adj_t1 <- get_adj_at(com_11_thresh_5_centr_single, t = 1)
g0 <- graph_from_adjacency_matrix(adj_t1, mode = "undirected", diag = FALSE)
fixed_layout <- layout_with_fr(g0)  # try layout_with_kk or layout_in_circle for alternatives


# 3. Number of time steps
n_time <- max(com_11_thresh_5_centr_single$meta$pers)

saveGIF({
  for (tt in 1:n_time) {
    plot_diffnet(
      com_11_thresh_2_centr_single,
      slices = tt,
      layout = fixed_layout,  # <- keeps positions fixed
      main = paste("Central Seeds - Time", tt)
    )
  }
}, movie.name = "/Users/dennisnientimp/Desktop/central_diffusion.gif", interval = 1)










#some try outs


#transforming my simulation into a class diffnet object


com_11_thresh_7_subc_uw_rw.2


#extract adoption times form matrix
adoption_times <- apply(com_11_thresh_7_subc_uw_rw.2, 1, function(x) {
  first_adopted <- which(x >= 0.5)  # Find first time a node adopts
  if (length(first_adopted) == 0) NA else min(first_adopted)  # Assign first adoption time or NA
})

summary(adoption_times)
any(is.na(adoption_times))


# Step 2: Reconstruct Network Snapshots
# Assuming you have an initial network (g_com_11_mat), you can construct a sequence of adjacency matrices:

network_slices <- list()
for (t in 1:100) {  # Loop through time periods
  network_slices[[t]] <- g_com_11_mat  # Assuming static structure
}


library(Matrix)
library(netdiffuseR)

# Convert each network slice into a sparse matrix
network_slices <- lapply(network_slices, function(mat) {
  as(mat, "dgCMatrix")  # Convert matrix to sparse format
})

# Ensure adoption_times is numeric and remove any NA values
adoption_times <- as.numeric(adoption_times)
adoption_times[is.na(adoption_times)] <- max(adoption_times, na.rm = TRUE) + 1  # Assign max+1 to missing values

# Check min and max adoption times
print(min(adoption_times, na.rm = TRUE))
print(max(adoption_times, na.rm = TRUE))


# Now create the diffnet object
diffnet_object <- new_diffnet(
  graph = network_slices, 
  toa = adoption_times, 
  name = "Com_11 Threshold 7 Subcentral Diffusion"
)





classify_adopters(com_11_thresh_2_rand_uw_rw.2) 
max(com_11_thresh_7_subc_uw_rw.2, na.rm = TRUE)  # Ensure at least some values reach 0.5




