library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)

# Load data
misid_data <- read_csv("misidentification_frequencies.csv")

# Extract genus from species name (assuming genus is the first word)
misid_data <- misid_data %>%
  mutate(Genus = word(Misidentification_Species_Name, 1)) %>%
  filter(!is.na(Genus) & Genus != "NA")

# Remove all rows containing 'Castilleja'
misid_data <- misid_data %>%
  filter(!grepl("Castilleja", Misidentification_Species_Name))

# Calculate total counts for each genus and species
misid_data <- misid_data %>%
  group_by(Genus, Misidentification_Species_Name) %>%
  mutate(Species_Total_Counts = sum(Total_Counts, na.rm = TRUE)) %>%
  ungroup()

# Calculate total counts for each genus
total_counts_per_genus <- misid_data %>%
  group_by(Genus) %>%
  summarise(Genus_Total_Counts = sum(Total_Counts, na.rm = TRUE))

# Select top 10 misidentified genera based on the total counts per genus
top_genera <- total_counts_per_genus %>%
  top_n(10, Genus_Total_Counts) %>%
  pull(Genus)

# Filter original data for top genera only
top_misid <- misid_data %>%
  filter(Genus %in% top_genera) %>%
  mutate(Group = paste(Genus, Misidentification_Species_Name, sep = "-"))

# Join the total counts per genus back to the top misidentified data
top_misid <- top_misid %>%
  left_join(total_counts_per_genus, by = "Genus")

# Build colour palette
ColourPalleteMulti <- function(df, group, subgroup){
  categories <- aggregate(as.formula(paste(subgroup, group, sep="~")), df, function(x) length(unique(x)))
  category.start <- scales::hue_pal(l = 100)(nrow(categories))
  category.end <- scales::hue_pal(l = 40)(nrow(categories))
  
  colours <- unlist(lapply(1:nrow(categories), function(i){
    colorRampPalette(colors = c(category.start[i], category.end[i]))(categories[i,2])
  }))
  return(colours)
}

colours <- ColourPalleteMulti(top_misid, "Genus", "Misidentification_Species_Name")

# Create plot with color gradient for each species within its genus
p <- ggplot(top_misid, aes(x = fct_reorder(Genus, Genus_Total_Counts), y = Species_Total_Counts, fill = Group)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = colours) +
  labs(x = "Misidentified Genus", y = "Total Counts", fill = "Species") +
  theme_minimal() +
  theme(legend.position = "bottom") + # Move legend to bottom
  theme(legend.text = element_text(size = 6)) + # Reduce text size
  guides(fill = guide_legend(ncol = 4)) # Increase number of columns

ggsave("output.svg", width = 8.5, height = 5.5, p)

p