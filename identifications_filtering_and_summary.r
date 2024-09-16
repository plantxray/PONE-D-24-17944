library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# Load data from CSV files
data <- read_csv("final_summary_castilleja_research_grade_all_species_new.csv")
taxa <- read_csv("taxa.csv")

# Create a named vector for species names
species_names <- setNames(taxa$scientificName, taxa$taxonID)

# Function to map taxon ID to species name
map_id_to_name <- function(id) {
  if (!is.numeric(id) || is.na(id) || !id %in% names(species_names)) {
    return(NA_character_)
  } else {
    return(species_names[as.character(id)])
  }
}

# Add Correct_Species_Name to the main data
data <- data %>%
  mutate(Correct_Species_Name = map_id_to_name(Correct_Species_ID))

# Function to extract genus and species from the scientific name
extract_genus_species <- function(name) {
  parts <- str_split(name, " ")[[1]]
  if (length(parts) >= 2) {
    return(paste(parts[1], parts[2]))
  } else {
    return(parts[1])
  }
}

# Convert Misidentified_species to character type
data$Misidentified_species <- as.character(data$Misidentified_species)

# Define a lookup table for subspecies exceptions
subspecies_exceptions <- tibble(
  Correct_Species_ID = c(50042, 60738, 282874, 70395, 856704, 133740, 314718, 523134, 60242, 55508, 49343, 160191, 128204, 123956, 282883, 1041229, 76148, 76149, 160211, 856709, 117498, 76153, 60480, 119119, 53915, 143148, 160175),
  Excluded_IDs = list(
    c(50043, 905977, 61232),
    c(61110, 1236202, 79680, 79681, 704677),
    c(160175, 239858, 468851, 239859),
    c(127664, 197080, 59331, 79684, 734740),
    c(856705),
    c(1196832, 1196830),
    c(314719),
    c(1445699),
    c(79685, 79686),
    c(61233, 79687, 79688),
    c(59012, 61109, 79689),
    c(239862, 239863),
    c(234936, 234938),
    c(1446381),
    c(1044615),
    c(1041231, 1041230),
    c(234941, 888100, 79691, 905976),
    c(1041276, 1041277, 1041280, 1041283),
    c(239866, 239867),
    c(963926, 963927),
    c(154246, 154247, 143869, 154248),
    c(239868, 239869, 239870),
    c(57269, 79694),
    c(239874, 239875),
    c(53916, 841248, 53917),
    c(743415, 314704, 314705),
    c(239858, 468851, 239859)
  )
)

# Define a lookup table for parent species exceptions
parent_species_exceptions <- tibble(
  Correct_Species_IDs = list(
    c(50043, 905977, 61232),
    c(61110, 1236202, 79680, 79681, 704677),
    c(160175, 239858, 468851, 239859),
    c(127664, 197080, 59331, 79684, 734740),
    c(856705),
    c(1196832, 1196830),
    c(314719),
    c(1445699),
    c(79685, 79686),
    c(61233, 79687, 79688),
    c(59012, 61109, 79689),
    c(239862, 239863),
    c(234936, 234938),
    c(1446381),
    c(1044615),
    c(1041231, 1041230),
    c(234941, 888100, 79691, 905976),
    c(1041276, 1041277, 1041280, 1041283),
    c(239866, 239867),
    c(963926, 963927),
    c(154246, 154247, 143869, 154248),
    c(239868, 239869, 239870),
    c(57269, 79694),
    c(239874, 239875),
    c(53916, 841248, 53917),
    c(743415, 314704, 314705),
    c(239858, 468851, 239859)
  ),
  Parent_ID = c(
    50042, 60738, 282874, 70395, 856704, 133740, 314718, 523134, 60242, 
    55508, 49343, 160191, 128204, 123956, 282883, 1041229, 76148, 76149, 
    160211, 856709, 117498, 76153, 60480, 119119, 53915, 143148, 160175
  )
)

# Function to check if a species should be excluded based on subspecies and parent species
should_exclude_species <- function(correct_id, misidentified_id) {
  excluded = FALSE
  # Check for subspecies exceptions
  if (!is.na(correct_id) && !is.na(misidentified_id)) {
    if (correct_id %in% subspecies_exceptions$Correct_Species_ID) {
      excluded_ids <- subspecies_exceptions$Excluded_IDs[subspecies_exceptions$Correct_Species_ID == correct_id][[1]]
      excluded = misidentified_id %in% excluded_ids
    }
    
    # Check for parent species exceptions
    if (!excluded && misidentified_id %in% parent_species_exceptions$Parent_ID) {
      correct_ids <- parent_species_exceptions$Correct_Species_IDs[parent_species_exceptions$Parent_ID == misidentified_id][[1]]
      excluded = correct_id %in% correct_ids
    }
  }
  
  return(excluded)
}

# Convert Misidentified_species to a list of numeric IDs
data$Misidentified_species_list <- map(data$Misidentified_species, convert_to_list)

# Apply the exclusion logic to each list element
data$Misidentified_species_list <- map2(data$Misidentified_species_list, data$Correct_Species_ID, function(species_list, correct_id) {
  if (is.na(species_list)) {
    return(NA)
  } else {
    # Check each ID and replace with NA if it should be excluded
    species_list <- map(species_list, function(misidentified_id) {
      if (should_exclude_species(correct_id, misidentified_id)) {
        return(NA)
      } else {
        return(misidentified_id)
      }
    })
    # Unlist to collapse the list of lists into a single list
    return(unlist(species_list))
  }
})

# Convert the list back to a comma-separated string, handling NA correctly
data$Misidentified_species <- map_chr(data$Misidentified_species_list, function(x) {
  if (is.na(x) || length(x) == 0) {
    return(NA_character_)
  } else {
    # Filter out NA values and convert to string
    valid_ids <- x[!is.na(x)]
    return(if (length(valid_ids) == 0) NA_character_ else paste(valid_ids, collapse = ", "))
  }
})

# Remove the list column 'Misidentified_species_list' before writing to CSV
data <- data %>% select(-Misidentified_species_list)

# Write the final data to a CSV file
write_csv(data, "final_summary_castilleja_research_grade_all_species_new_no_subspecies_or_parents.csv")



#Quantification:

# Total number of Castilleja identifications (all entries)
total_castilleja_identifications <- nrow(data)

# Apply the exclusion logic to the dataset for correct identifications
data_separated <- data_separated %>%
  mutate(
    Exclude_Based_On_Exceptions = mapply(should_exclude_species, Correct_Species_ID, Misidentified_species),
    Is_Correct_Identification = is.na(Misidentified_species) & !Exclude_Based_On_Exceptions
  )

# Number of correct identifications
correct_identifications <- sum(data_separated$Is_Correct_Identification, na.rm = TRUE)

# Identified as 'Castilleja' on genus level (taxon ID 49340)
castilleja_genus_identifications <- sum(data$Correct_Species_ID == 49340 | data$Misidentified_species == 49340, na.rm = TRUE)

# Identified on higher taxonomy level
higher_taxonomy_ids <- c(47126, 47125, 47322, 47124, 211194, 902069, 48151, 48460)
higher_taxonomy_identifications <- sum(data$Correct_Species_ID %in% higher_taxonomy_ids | data$Misidentified_species %in% higher_taxonomy_ids, na.rm = TRUE)

# Identified as different species
different_species_identifications <- sum(data$Correct_Species_ID != data$Misidentified_species & !is.na(data$Misidentified_species), na.rm = TRUE)

# Count of misidentified species before filtering
pre_filter_count <- nrow(data_separated)

# Apply your filtering logic here (not included in this snippet)

# Count of misidentified species after filtering
post_filter_count <- nrow(data_separated) # Assuming data_separated is modified by the filtering logic

# Number of entries filtered out
filtered_count <- pre_filter_count - post_filter_count

# Create a summary table
summary_stats <- tibble(
  Total_Identifications = total_castilleja_identifications,
  Correct_Identifications = correct_identifications,
  Genus_Level_Identifications = castilleja_genus_identifications,
  Higher_Taxonomy_Identifications = higher_taxonomy_identifications,
  Different_Species_Identifications = different_species_identifications,
  Filtered_Out = filtered_count
)

# Display summary table
print(summary_stats)

# Optionally, save the summary table to a CSV file
write_csv(summary_stats, "summary_statistics.csv")

# Load Castilleja taxon IDs
castilleja_taxa <- read_csv("Castilleja taxon IDs.csv")
castilleja_taxon_ids <- castilleja_taxa$taxon_id

# Check if a taxon ID belongs to Castilleja
is_castilleja <- function(taxon_id) {
  return(taxon_id %in% castilleja_taxon_ids)
}

# Apply logic to data_separated
data_separated <- data_separated %>%
  mutate(
    Identified_as_different_Castilleja = Correct_Species_ID != Misidentified_species & 
                                         !is.na(Misidentified_species) & 
                                         is_castilleja(Misidentified_species),
    Identified_as_different_genus = Correct_Species_ID != Misidentified_species & 
                                    !is.na(Misidentified_species) & 
                                    !is_castilleja(Misidentified_species)
  )

# Calculate the breakdown
different_castilleja_species_identifications <- sum(data_separated$Identified_as_different_Castilleja, na.rm = TRUE)
different_genus_identifications <- sum(data_separated$Identified_as_different_genus, na.rm = TRUE)

# Update summary table
summary_stats <- summary_stats %>%
  mutate(
    Identified_as_different_Castilleja_species = different_castilleja_species_identifications,
    Identified_as_different_genus = different_genus_identifications
  )

# Display updated summary table
print(summary_stats)

# Optionally, save the updated summary table to a CSV file
write_csv(summary_stats, "updated_summary_statistics.csv")

# Calculating the actual overlaps
# Overlap between Correct Identifications and Genus-Level Identifications
overlap_correct_genus_level <- sum(data_separated$Is_Correct_Identification & (data$Correct_Species_ID == 49340 | data$Misidentified_species == 49340), na.rm = TRUE)

# Overlap between Correct Identifications and Different Castilleja Species Identifications
overlap_correct_different_castilleja <- sum(data_separated$Is_Correct_Identification & data_separated$Identified_as_different_Castilleja, na.rm = TRUE)

# Overlap between Genus-Level Identifications and Different Castilleja Species Identifications
overlap_genus_level_different_castilleja <- sum((data$Correct_Species_ID == 49340 | data$Misidentified_species == 49340) & data_separated$Identified_as_different_Castilleja, na.rm = TRUE)

# Overlap among all three categories
overlap_all_three <- sum(data_separated$Is_Correct_Identification & (data$Correct_Species_ID == 49340 | data$Misidentified_species == 49340) & data_separated$Identified_as_different_Castilleja, na.rm = TRUE)

# Adding the overlap calculations to the summary_stats
summary_stats <- summary_stats %>%
  mutate(
    Overlap_Correct_Genus_Level = overlap_correct_genus_level,
    Overlap_Correct_Different_Castilleja = overlap_correct_different_castilleja,
    Overlap_Genus_Level_Different_Castilleja = overlap_genus_level_different_castilleja,
    Overlap_All_Three = overlap_all_three
  )

# Save table to a CSV file
write_csv(summary_stats, "updated_summary_statistics_with_overlaps.csv")