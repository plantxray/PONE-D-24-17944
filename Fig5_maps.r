library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(rnaturalearth)
library(stringr)
library(rgeos)

# Function to create a convex hull for a set of points, with additional cleanup
create_convex_hull <- function(df) {
  if (nrow(df) < 3) {
    warning("create_convex_hull: Input dataframe has less than 3 rows. Not enough points for a convex hull.")
    return(NULL)
  }
  
  
# Convert data to spatial data frame
convert_to_sf <- function(df, label) {
  df %>% 
    select(Long, Lat) %>% 
    st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
    mutate(Label = label)
}

  # Clean the data by removing duplicate/near-duplicate points
  df_cleaned <- df %>%
    st_coordinates() %>%
    as.data.frame() %>%
    distinct(X, Y, .keep_all = TRUE) %>%
    st_as_sf(coords = c("X", "Y"), crs = st_crs(df))

  if (nrow(df_cleaned) < 3) {
    warning("create_convex_hull: Dataframe cleaned to less than 3 unique points. Not enough unique points after cleaning.")
    return(NULL)
  }

  points <- st_coordinates(df_cleaned)
  chull_indices <- chull(points)
  chull_points <- points[chull_indices, ]
  chull_points <- rbind(chull_points, chull_points[1, ])  # Close the polygon
  hull_polygon <- st_polygon(list(chull_points))
  st_sfc(hull_polygon, crs = st_crs(df_cleaned))
}

# Function to check if points are within a polygon
check_points_inside <- function(sf_points, polygon) {
  if (is.null(polygon) || nrow(sf_points) == 0) {
    return(rep(FALSE, nrow(sf_points)))
  }
  st_intersects(sf_points, polygon, sparse = FALSE) %>% apply(1, any)
}

# Load taxa data to get species names and IDs
taxa_data <- read_csv("taxa.csv")

# Load the observation data
castilleja_data <- read_csv("final_summary_castilleja_research_grade_all_species_new_with_other_genera.csv")
trifolium_data <- read_csv("Triphysaria versicolor misidentifications dataset for maps.csv")

# Load pairs for which maps will be created
pairs_for_maps <- read_csv('pairs_for_maps_miniata_coccineus.csv')

# Function to generate maps for each pair and create an output table
generate_maps_for_pairs <- function(pair, taxa_data, castilleja_data, trifolium_data) {
  taxon1_ids <- taxa_data %>% 
    filter(str_detect(scientificName, pair$Taxon1)) %>% 
    pull(taxonID)
  taxon2_ids <- taxa_data %>% 
    filter(str_detect(scientificName, pair$Taxon2)) %>% 
    pull(taxonID)
  
  taxon1_observations <- castilleja_data %>%
    filter(Correct_Species_ID %in% taxon1_ids & !is.na(Lat) & !is.na(Long))
  taxon2_observations <- castilleja_data %>%
    filter(Correct_Species_ID %in% taxon2_ids & !is.na(Lat) & !is.na(Long))

  misidentified_observations <- castilleja_data %>%
    filter(Correct_Species_ID %in% taxon1_ids & Misidentified_species %in% taxon2_ids & !is.na(Lat) & !is.na(Long))

  sf_taxon1 <- convert_to_sf(taxon1_observations, pair$Taxon1)
  sf_taxon2 <- convert_to_sf(taxon2_observations, pair$Taxon2)
  sf_misidentified <- convert_to_sf(misidentified_observations, "Misidentified")

  hull_taxon1 <- create_convex_hull(sf_taxon1)
  if (is.null(hull_taxon1)) {
    warning(paste("generate_maps_for_pairs: Hull for Taxon1 (", pair$Taxon1, ") could not be created.", sep=""))
  }

  hull_taxon2 <- create_convex_hull(sf_taxon2)
  if (is.null(hull_taxon2)) {
    warning(paste("generate_maps_for_pairs: Hull for Taxon2 (", pair$Taxon2, ") could not be created.", sep=""))
  }

  if (is.null(hull_taxon1) || is.null(hull_taxon2)) {
    return(NULL)
  }

  taxon1_inside_hull1 <- check_points_inside(sf_taxon1, hull_taxon1)
  taxon1_inside_hull2 <- check_points_inside(sf_taxon1, hull_taxon2)
  taxon2_inside_hull1 <- check_points_inside(sf_taxon2, hull_taxon1)
  taxon2_inside_hull2 <- check_points_inside(sf_taxon2, hull_taxon2)
  misid_inside_hull1 <- check_points_inside(sf_misidentified, hull_taxon1)
  misid_inside_hull2 <- check_points_inside(sf_misidentified, hull_taxon2)

  taxon1_overlap <- taxon1_inside_hull1 & taxon1_inside_hull2
  taxon2_overlap <- taxon2_inside_hull1 & taxon2_inside_hull2
  misid_overlap <- misid_inside_hull1 & misid_inside_hull2

  counts <- c(
    sum(taxon1_inside_hull1), sum(taxon1_inside_hull2), sum(taxon1_overlap),
    sum(taxon2_inside_hull1), sum(taxon2_inside_hull2), sum(taxon2_overlap),
    sum(misid_inside_hull1), sum(misid_inside_hull2), sum(misid_overlap)
  )

  world <- ne_countries(scale = "large", returnclass = "sf")

map_title <- paste("Habitat Overlap and Misidentification Locations for", pair$Taxon1, "and", pair$Taxon2)
p <- ggplot() +
  geom_sf(data = world, fill = "#f7edd1", color = "black", size = 0.1) +
  geom_sf(data = st_as_sf(hull_taxon1), fill = NA, color = "blue", size = 0.5) +
  geom_sf(data = st_as_sf(hull_taxon2), fill = NA, color = "red", size = 0.5) +
  geom_sf(data = sf_misidentified, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = map_title,
       caption = "Habitat areas are outlined. Misidentifications are marked as points.") +
  scale_color_viridis_d(name = "Misidentifications", breaks = "Misidentified") +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "#F0F8FF", color = NA)
  )

  file_name <- paste0(gsub(" ", "_", pair$Taxon1), "_", gsub(" ", "_", pair$Taxon2), "_Map (NEW).svg")
  ggsave(file_name, width = 11, height = 6, p)

  result_df <- data.frame(
    Pair = paste(pair$Taxon1, pair$Taxon2),
    Taxon1_Inside_Hull1 = counts[1], Taxon1_Inside_Hull2 = counts[2], Taxon1_In_Overlap = counts[3],
    Taxon2_Inside_Hull1 = counts[4], Taxon2_Inside_Hull2 = counts[5], Taxon2_In_Overlap = counts[6],
    Misid_Inside_Hull1 = counts[7], Misid_Inside_Hull2 = counts[8], Misid_In_Overlap = counts[9]
  )

  return(list(Map = p, Data = result_df))
}

summary_list <- lapply(seq_len(nrow(pairs_for_maps)), function(i) {
  generate_maps_for_pairs(pairs_for_maps[i, ], taxa_data, castilleja_data, trifolium_data)
})

summary_df <- do.call(rbind, lapply(summary_list, function(x) if (!is.null(x)) x$Data))

print(summary_df)
View(summary_df)

# Save summary_df to a CSV file
write.csv(summary_df, "Castilleja miniata vs Haemanthus coccineus (NEW).csv", row.names = FALSE)