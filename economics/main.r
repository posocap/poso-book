# Load required libraries
library(ggplot2)
library(dplyr)

# Data Generating Function ------------------------------------------------
generate_half_parabola_data <- function(a, b, c=1, num_points, upward = TRUE) {
  x <- seq(0, 10, length.out = num_points)  # Adjust the range as needed
  if (upward) {
    y <- a * x^2 + b * x + c
  } else {
    y <- -a * x^2 - b * x - c
  }
  return(y)
}

# Sample data (replace with your own dataset)
data <- data.frame(
  Complexity  = seq_len(11),
  Rationality = c(generate_half_parabola_data(1,2,1,11)),
  Impact      = c(rev((generate_half_parabola_data(1,2,1,11))))# - mean(c(rev((generate_half_parabola_data(1,2,1,11)))))
)

# Calculate the maximum value of Rationality for adjusting the second Y-axis
max_Rationality <- max(data$Rationality)

# Plot --------------------------------------------------------------------
# Create a ggplot object with legend below the x-axis
p <- ggplot(data, aes(x = factor(Complexity))) +
  
  # Add Rationality line plot
  geom_line(aes(y = Rationality, group = 1, color = "Rationality"), size = 1.2, show.legend = TRUE) +
  
  # Add Unemployment Rate line plot
  geom_line(aes(y = Impact * (max_Rationality/max(Impact)), group = 1, color = "Impact of Intervention"), size = 1.2, linetype = "dashed", show.legend = TRUE) +
  #geom_line(aes(y = (Impact - mean(Impact)) * (max_Rationality / (max(Impact) - mean(Impact))), group = 1, color = "Impact of Intervention"), size = 1.2, linetype = "dashed", show.legend = TRUE) +
  
  # Customize appearance
  labs(
    title = "Fig X. ",
    subtitle = "Decision Making",
    x = "Complexity",
    y = "Rationality"
  ) +
  
  scale_color_manual(
    values = c("Rationality" = "blue", "Impact of Intervention" = "red"),
    name = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    
    # Move the legend below the x-axis
    legend.position = "bottom",
    legend.box = "horizontal",
    
    # Format Complexity labels as integers and rotate x-axis labels
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    
    # Bold border on the first Y-axis and x-axis
    axis.line.x = element_line(size = 1, color = "black", lineend = "square"),
    axis.line.y.left = element_line(size = 1, color = "black", lineend = "square"),
    
    # Remove gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  scale_y_continuous(
    sec.axis = sec_axis(~. * (max(data$Impact)/max_Rationality), name = "Impact of Intervention"),
    limits = c(0, max_Rationality)
  )

# Display the graph
print(p)
