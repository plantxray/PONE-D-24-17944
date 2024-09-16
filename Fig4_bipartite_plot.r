library(dplyr)
library(readr)
library(igraph)
library(ggraph)
library(tidyr)

# Load the data
misidentification_data <- read_csv("misidentification_analysis_with_observation_IDs_higher_taxonomy_and_subspecies_excluded.csv")

# Expand the rows for each misidentification
misidentification_data_expanded <- misidentification_data %>%
  mutate(row_id = row_number()) %>%
  separate_rows(Misidentifications_With_Names, Misidentifications_Count, sep = "\\|") %>%
  mutate(Misidentifications_Count = as.numeric(Misidentifications_Count))

# Filter based on the count criteria (e.g., 250)
misidentification_data_filtered <- misidentification_data_expanded %>%
  filter(Misidentifications_Count >= 50)

# Append a prefix to Misidentifications_With_Names to distinguish them
misidentification_data_filtered <- misidentification_data_filtered %>%
  mutate(Misidentifications_With_Names = paste("MisID:", Misidentifications_With_Names))

# Create an edge list with weights for the bipartite graph
edge_list <- misidentification_data_filtered %>%
  select(Correct_Species_Name, Misidentifications_With_Names, Misidentifications_Count)

# Specifically remove rows where Misidentifications_With_Names is the string "NA"
edge_list <- edge_list %>%
  filter(Misidentifications_With_Names != "MisID: NA")

# Keep only rows where 'Castilleja' occurs in 'Correct_Species_Name'
edge_list <- edge_list %>%
  filter(grepl("Castilleja", Correct_Species_Name))

# Remove rows where 'Castilleja' occurs in 'Misidentifications_With_Names'
#edge_list <- edge_list %>%
  #filter(!grepl("Castilleja", Misidentifications_With_Names))

# Create an igraph object from the edge list
network <- graph_from_data_frame(d = edge_list, directed = FALSE, vertices = NULL)

# Define types for bipartite layout
correct_species_names <- unique(edge_list$Correct_Species_Name)
V(network)$type <- ifelse(V(network)$name %in% correct_species_names, TRUE, FALSE)

# Create a bipartite layout
LO <- layout_as_bipartite(network, types = V(network)$type)
# Rotate and flip the layout
LO_rotated <- as.data.frame(LO)
names(LO_rotated) <- c("x", "y")
LO_rotated$x <- LO_rotated$y  # Swap coordinates
LO_rotated$y <- -LO[, 1]  # Negate original x coordinate

# Plot the network with ggraph using the rotated and flipped layout
p <- ggraph(network, layout = 'manual', x = LO_rotated$x, y = LO_rotated$y) +
  geom_edge_link(aes(width = Misidentifications_Count), edge_colour = "gray") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), hjust = 0, vjust = 1, check_overlap = TRUE) +
  theme_graph() +
  labs(title = "Network of Species Misidentifications",
       subtitle = "Each edge represents a misidentification occurrence",
       edge_width = "Number of misidentifications") +
  theme(legend.position = "none")  

# Save the plot
ggsave("Bipartite plot.svg", width = 6, height = 6, p)

p