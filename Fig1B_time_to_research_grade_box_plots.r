library(dplyr)
library(readr)
library(ggplot2)

# Function to convert time to a human-readable format
convert_to_human_readable <- function(time_in_days) {
  years <- floor(time_in_days / 365)
  months <- floor((time_in_days %% 365) / 30)
  days <- floor(time_in_days %% 30)
  
  parts <- c()
  if (years > 0) parts <- c(parts, paste(years, "year", if(years > 1) "s" else ""))
  if (months > 0) parts <- c(parts, paste(months, "month", if(months > 1) "s" else ""))
  if (days > 0) parts <- c(parts, paste(days, "day", if(days > 1) "s" else ""))
  
  if (length(parts) == 0) return("less than a day")
  paste(parts, collapse = ", ")
}

# Load the observation-level data
observation_data <- read_csv("sorted_trimmed_castilleja_data.csv")

# Exclude data points where Time_to_Quality_Grade_Update is greater than 1000
filtered_data <- observation_data %>%
  filter(Time_to_Quality_Grade_Update <= 1000)

# Calculate summary statistics for Time_to_Quality_Grade_Update grouped by Computer_Vision_Used
summary_stats <- filtered_data %>%
  group_by(Computer_Vision_Used) %>%
  summarise(
    count = n(),
    median = median(Time_to_Quality_Grade_Update, na.rm = TRUE),
    mean = mean(Time_to_Quality_Grade_Update, na.rm = TRUE),
    sd = sd(Time_to_Quality_Grade_Update, na.rm = TRUE),
    min = min(Time_to_Quality_Grade_Update, na.rm = TRUE),
    max = max(Time_to_Quality_Grade_Update, na.rm = TRUE),
    IQR = IQR(Time_to_Quality_Grade_Update, na.rm = TRUE)
  ) %>%
  mutate(
    median_readable = sapply(median, convert_to_human_readable),
    mean_readable = sapply(mean, convert_to_human_readable),
    sd_readable = sapply(sd, convert_to_human_readable),
    min_readable = sapply(min, convert_to_human_readable),
    max_readable = sapply(max, convert_to_human_readable),
    IQR_readable = sapply(IQR, convert_to_human_readable)
  )

# Perform t-test to check for significant difference
t_test_result <- t.test(Time_to_Quality_Grade_Update ~ Computer_Vision_Used, data = filtered_data)

# Save the human-readable summary statistics to a text file
write.table(summary_stats, file = "summary_stats_time_to_qgu_grouped_by_computer_vision_human_readable.txt", sep = "\t", row.names = FALSE)

# Append significance test results to the file
cat("\n\nSignificance Test Results:\n", file = "summary_stats_time_to_qgu_grouped_by_computer_vision_human_readable.txt", append = TRUE)
cat("p-value:", format.pval(t_test_result$p.value, digits = 3), "\n", file = "summary_stats_time_to_qgu_grouped_by_computer_vision_human_readable.txt", append = TRUE)
cat("Significant difference:", t_test_result$p.value < 0.05, "\n", file = "summary_stats_time_to_qgu_grouped_by_computer_vision_human_readable.txt", append = TRUE)

# Define Y-axis labels and corresponding values in days
time_labels <- c("10 sec", "1 min", "10 min", "30 min", "1 hour", "12 hours", "1 day", "1 week", "1 month", "6 months", "1 year", "2 years", "3 years")
time_values <- c(10/86400, 1/1440, 10/1440, 30/1440, 1/24, 12/24, 1, 7, 30, 182.5, 365, 730, 1095)

# Create a box plot for Time_to_Quality_Grade_Update grouped by Computer_Vision_Used with log scale
boxplot_time_to_qgu <- ggplot(filtered_data, aes(x = factor(Computer_Vision_Used), y = Time_to_Quality_Grade_Update)) +
  geom_boxplot() +
  scale_y_log10(
    breaks = time_values,
    labels = time_labels
  ) +  # Apply log scale to the y-axis with custom labels
  labs(x = "Computer Vision Used (TRUE = 1, FALSE = 0)",
       y = "Time to Quality Grade Update") +
  theme_minimal()

# Save the box plot as an SVG file with specified dimensions
ggsave("boxplot_time_to_qgu_grouped_by_computer_vision_log_scale.svg", boxplot_time_to_qgu, width = 4.25, height = 4.25, units = "in")