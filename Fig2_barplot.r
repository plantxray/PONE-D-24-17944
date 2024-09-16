library(ggplot2)
library(tidyr)

# Read the data from the CSV file
data <- read.csv("updated_summary_statistics_for_bar_plot.csv")

# Transform data from wide to long format for easier plotting
long_data <- gather(data, key = "Category", value = "Count")

# Set the factor levels to the order in the CSV
long_data$Category <- factor(long_data$Category, levels = names(data))

# Generate the bar plot with values displayed
p <- ggplot(long_data, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, color = "black", size = 3.5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    plot.title = element_blank()
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("barplot.svg", width = 4.25, height = 4.25, p)

p