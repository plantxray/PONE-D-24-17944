library(dplyr)
library(readr)
library(ggplot2)

# Load the observation-level data
observation_data <- read_csv("corrected_castilleja_research_grade_with_times.csv")

# Group by Computer_Vision_Used and Correct_Species_identified, then calculate percentages and totals
species_identified_percentages <- observation_data %>%
  group_by(Computer_Vision_Used) %>%
  summarise(total = n()) %>%
  left_join(
    observation_data %>%
      group_by(Computer_Vision_Used, Correct_Species_identified) %>%
      summarise(count = n()),
    by = "Computer_Vision_Used"
  ) %>%
  mutate(percentage = (count / total) * 100)

# Create a bar plot for Correct_Species_identified percentages grouped by Computer_Vision_Used
barplot_species_identified <- ggplot(species_identified_percentages, aes(x = factor(Computer_Vision_Used), y = percentage, fill = Correct_Species_identified)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%\n(", count, ")")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3) +  # Add percentage and count labels on top of the bars
  scale_y_continuous(limits = c(0, 100)) +  # Set Y-axis scale from 0 to 100
  labs(x = "Computer Vision Used",
       y = "Percentage",
       fill = "Correct Species Identified") +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Move legend to the bottom
  scale_x_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes")) +
  scale_fill_discrete(labels = c("FALSE" = "No", "TRUE" = "Yes"))

# Save the bar plot as an SVG file with specified dimensions
ggsave("barplot_species_identified_percentage_and_count_by_computer_vision.svg", barplot_species_identified, width = 4.25, height = 4.25, units = "in")