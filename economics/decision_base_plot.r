# Libraries ---------------------------------------------------------------
if (!require(pacman)) { install.packages("pacman"); library(pacman) }  
p_load(dplyr, ggplot2)

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

# Synthetic Data
a <- 2
b <- 0
c <- 2
l <- 11
data <- data.frame(
  Complexity  = seq_len(l),
  Rationality = c(generate_half_parabola_data(a, b, c, l))/max(c(generate_half_parabola_data(a, b, c, l))),
  Impact      = c(rev((generate_half_parabola_data(a, b, -c, l))))/max(c(generate_half_parabola_data(a, b, c, l)))
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
    x = "Inverse Complexity",
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
  
  # Modify the scale_y_continuous for Impact of Intervention
  # Override the labels for the second Y-axis (Impact of Intervention)
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ 2 * (. - min(data$Impact)) / (max(data$Impact) - min(data$Impact)) - 1,
      name = "Impact of Intervention",
      labels = scales::number_format(accuracy = 0.1)
    )
  )

# Display the graph
print(p)