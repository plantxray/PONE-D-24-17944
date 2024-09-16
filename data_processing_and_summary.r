library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)

# Load species data from CSV
species_data <- read_csv("species_taxon_ids.csv")

# Function to fetch observations for a specific taxon
fetch_observations <- function(taxon_id, page = 1, per_page = 200) {
  response <- GET(
    url = "https://api.inaturalist.org/v1/observations",
    query = list(
      taxon_id = taxon_id,
      page = page,
      per_page = per_page,
      quality_grade = "research"
    )
  )
  data <- content(response, as = "parsed", type = "application/json")
  
  return(list(total_results = data$total_results, results = data$results))
}

# Function to analyze all identifications and calculate time to research grade
analyze_all_identifications <- function(observation, species_id) {
  tryCatch({
    if (is.null(observation) || !is.list(observation)) {
      cat("Observation is NULL or not a list\n")
      return(NULL)
    }

    cat("Processing observation ID:", ifelse(is.null(observation$id), "NULL", observation$id), "\n")

    idents <- observation$identifications
    
    lat <- if (!is.null(observation$geojson) && length(observation$geojson$coordinates) >= 2) {
      observation$geojson$coordinates[[2]] # Latitude
    } else {
      NA_real_
    }
    long <- if (!is.null(observation$geojson) && length(observation$geojson$coordinates) >= 1) {
      observation$geojson$coordinates[[1]] # Longitude
    } else {
      NA_real_
    }
    
    misidentified_species <- if(length(idents) > 0) {
      unique(unlist(lapply(idents, function(ident) {
        if(!is.null(ident$taxon_id) && ident$taxon_id != species_id && !is.na(ident$taxon_id)) {
          return(as.character(ident$taxon_id))
        }
      })))
    } else {
      character(0)
    }
    misidentified_species <- na.omit(misidentified_species)
    misidentified_species_string <- if(length(misidentified_species) > 0) paste(misidentified_species, collapse = ",") else NA_character_
    
    is_correct_species <- any(sapply(idents, function(ident) !is.null(ident$taxon_id) && !is.na(ident$taxon_id) && ident$taxon_id == species_id))
    
    computer_vision_used <- if(length(idents) > 0) {
  !is.null(idents[[1]]$vision) && !is.na(idents[[1]]$vision) && idents[[1]]$vision
} else {
  FALSE
}
    
    time_created <- ymd_hms(observation$created_at, quiet = TRUE)
    time_quality_grade_updated <- ymd_hms(observation$updated_at, quiet = TRUE)
    
    time_to_quality_grade_update <- if(!is.na(time_created) && !is.na(time_quality_grade_updated)) {
      as.numeric(difftime(time_quality_grade_updated, time_created, units = "days"))
    } else {
      NA_real_
    }
    
    result <- data.frame(
      observation_id = ifelse(is.null(observation$id), NA_character_, as.character(observation$id)),
      Correct_Species_identified = ifelse(is.logical(is_correct_species), is_correct_species, NA),
      Correct_Species_ID = as.character(species_id),
      Misidentified_species = misidentified_species_string,
      Lat = lat,
      Long = long,
      Computer_Vision_Used = ifelse(is.logical(computer_vision_used), computer_vision_used, NA),
      Time_Created = as.character(time_created),
      Time_Quality_Grade_Updated = as.character(time_quality_grade_updated),
      Time_to_Quality_Grade_Update = time_to_quality_grade_update,
      Quality_Grade = ifelse(is.null(observation$quality_grade), NA_character_, as.character(observation$quality_grade)),
      stringsAsFactors = FALSE
    )
    
    cat("Processed observation successfully\n")
    return(result)
  }, error = function(e) {
    cat("Error in analyze_all_identifications:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Main execution block
all_summaries <- list()
total_species <- nrow(species_data)

for (i in 1:total_species) {
  species_id <- species_data$taxon_id[i]
  
  all_observations <- list()
  page <- 1
  total_results <- 0
  
  repeat {
    fetched_data <- fetch_observations(species_id, page)
    if (page == 1) total_results <- fetched_data$total_results
    all_observations <- c(all_observations, fetched_data$results)
    page <- page + 1
    if (length(fetched_data$results) == 0 || length(all_observations) >= total_results) {
      break
    }
    Sys.sleep(1)
  }
  
  cat("Analyzing", length(all_observations), "observations for species", species_id, "\n")
  identification_analysis <- lapply(all_observations, analyze_all_identifications, species_id)
  identification_analysis <- Filter(Negate(is.null), identification_analysis)  # Remove NULL results
  cat("Successfully analyzed", length(identification_analysis), "observations\n")
  
  if (length(identification_analysis) > 0) {
    all_summaries[[i]] <- do.call(rbind, identification_analysis)
    cat("Added summary for species", species_id, "with", nrow(all_summaries[[i]]), "rows\n")
  } else {
    cat("No valid observations for species", species_id, "\n")
  }
  
  cat("Processed species", i, "of", total_species, "\n\n")
}

# Combine all summaries into a single data frame
final_summary_df <- bind_rows(all_summaries)

cat("Final summary has", nrow(final_summary_df), "rows\n")

# Save the results
output_file <- "final_summary_species_research_grade_with_times.csv"
write_csv(final_summary_df, output_file)

cat("Data processing complete. Results saved to", output_file, "\n")


# Create summary table
# Load the output data
data <- read_csv("final_summary_species_research_grade_with_times.csv")

# Calculate the summary statistics by species
species_summary <- data %>%
  group_by(Correct_Species_ID) %>%
  summarise(
    Total_Observations = n(),
    Percent_Computer_Vision_Used_TRUE = sum(Computer_Vision_Used == TRUE, na.rm = TRUE) / Total_Observations * 100,
    Percent_Computer_Vision_Used_FALSE = sum(Computer_Vision_Used == FALSE, na.rm = TRUE) / Total_Observations * 100,
    Median_Time_to_Quality_Grade_Update = median(Time_to_Quality_Grade_Update, na.rm = TRUE)
  )

# Calculate the summary statistics for the entire dataset using genus taxon ID
overall_summary <- data %>%
  summarise(
    Correct_Species_ID = 49340,  # Use the genus taxon ID for overall summary
    Total_Observations = n(),
    Percent_Computer_Vision_Used_TRUE = sum(Computer_Vision_Used == TRUE, na.rm = TRUE) / Total_Observations * 100,
    Percent_Computer_Vision_Used_FALSE = sum(Computer_Vision_Used == FALSE, na.rm = TRUE) / Total_Observations * 100,
    Median_Time_to_Quality_Grade_Update = median(Time_to_Quality_Grade_Update, na.rm = TRUE)
  )

# Combine species summary and overall summary
combined_summary <- bind_rows(species_summary, overall_summary)

# Save the summary table to a CSV file
output_file <- "species_summary_with_overall.csv"
write_csv(combined_summary, output_file)

cat("Summary table with overall statistics saved to", output_file, "\n")
