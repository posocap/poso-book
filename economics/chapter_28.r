# Options -----------------------------------------------------------------
set.seed(123)
options(scipen = 300)

# Libraries ---------------------------------------------------------------
if (!require(pacman)) { install.packages("pacman"); library(pacman) }  

p_load(devtools, ggplot2, grid, gridExtra, Hmisc, stringr)

install_github("R-CoderDotCom/econocharts")
require(econocharts)

# Theme -------------------------------------------------------------------
theme_supply_demand <- function() {
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 32, 
                              margin = margin(b = 20, unit = "pt")),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    plot.background = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.line = element_line(arrow = arrow(length = unit(2, "mm"), 
                                           ends = "last", type = "closed")),
    axis.ticks = element_line(color = "black"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(40, 40, 40, 40, unit = "pt")
  )
}


# Chart 18 ----------------------------------------------------------------
data <- data.frame(
  x = seq(0, 10, length.out = 100)
)
data$y1 <- 10 - (data$x)^2 * 0.1
data$y2 <- (data$x)^2 * 0.1

# Assuming 'data' is already defined
p <- ggplot(data) +
  geom_line(aes(x, y1), colour = "white") +
  geom_line(aes(x, y2), colour = "white") +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 2),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),  # Hide axis text
    axis.title = element_text(size = 18, face = "bold"),  # Match axis title size and boldness
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Match plot title size and boldness
    plot.margin = unit(c(1, 1, 1, 1), "lines")
  ) +
  labs(
    title = "← Alan's Quantity of Steaks",  # Adjust title here
    x = "Milton's Quantity of Steaks →",
    y = "Milton's Quantity of Milkshakes →"
  ) +
  scale_x_continuous(limits = c(0, 10), expand = expansion(mult = c(0, 0.05)), labels = NULL) +
  scale_y_continuous(limits = c(0, 10), expand = expansion(mult = c(0.05, 0)), labels = NULL,
                     sec.axis = sec_axis(~ ., name = "Alan's Quantity of Milkshakes →", labels = NULL)) +
  coord_fixed(ratio = 1)

print(p)

plot_title <- textGrob("Chart 18: Empty Edgeworth Box Plot", 
                       gp = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(p), heights = c(1, 10))

ggsave("plots/chart18.png", final_plot, 
       width  = 10, 
       height = 11, 
       units  = "in", 
       dpi    = 300)


# Chart 19 ----------------------------------------------------------------
# Use PowerPoint Instead

# Chart 20 ----------------------------------------------------------------
# Use PowerPoint Version Instead

# x <- seq(0.1, 10, length.out = 100)
# y <- seq(0.1, 10, length.out = 100)
# 
# grid       <- expand.grid(x = x, y = y)
# grid$label <- paste0("IC", rep(seq(1,10,1), each = 1000))
# 
# # Cobb-Douglas utility functions for Milton and Alan
# grid$U_Milton <- grid$x^0.3 + grid$y^0.5
# grid$U_Alan   <- (10 - grid$x)^0.3 + (10 - grid$y)^0.5
# 
# p <- ggplot(grid, aes(x = x, y = y)) +
#   geom_contour(aes(z = U_Milton, color = "Milton"), breaks = quantile(grid$U_Milton, 
#                    probs = seq(0.1, 0.9, by = 0.2)), size = 1) +
#   geom_contour(aes(z = U_Alan,   color = "Alan"),   breaks = quantile(grid$U_Alan,   
#                    probs = seq(0.1, 0.9, by = 0.2)), size = 1) +
#   scale_color_manual(values = c("Milton" = "darkblue", "Alan" = "darkred")) +
#   theme_minimal() +
#   theme(
#     panel.border = element_rect(colour = "black", fill = NA, size = 2),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.text = element_blank(),
#     axis.title.x = element_text(size = 12, face = "bold"),
#     axis.title.y = element_text(size = 12, face = "bold"),
#     plot.title = element_text(size   = 12, face = "bold", hjust = 0)
#   ) +
#   labs(
#     title = "← Alan's Quantity of Steaks",
#     x = "Milton's Quantity of Steaks →",
#     y = "Milton's Quantity of Shakes →"
#   ) +
#   scale_x_continuous(limits = c(0, 10), expand = expansion(mult = c(0, 0.05)), labels = NULL) +
#   scale_y_continuous(limits = c(0, 10), expand = expansion(mult = c(0.05, 0)), labels = NULL,
#                      sec.axis = sec_axis(~ ., name = "Alan's Quantity of Shakes →", labels = NULL)) +
#   coord_fixed(ratio = 1)
# 
# p
# 
# final_plot <- grid.arrange(plot_title, ggplotGrob(p), heights = c(1, 10))
# plot_title <- textGrob("Chart 20: Contract Curve (Edgeworth Box)", 
#                        gp = gpar(fontface = "bold", fontsize = 32),
#                        just = "center")
# 
# final_plot <- grid.arrange(plot_title, ggplotGrob(p), heights = c(1, 10))
# final_plot
# 
# ggsave("plots/chart20.png", final_plot, 
#        width  = 10, 
#        height = 11, 
#        units  = "in", 
#        dpi    = 300)


# Chart 21 ----------------------------------------------------------------
# Fake data for caricaturist example
x <- 1:24
y <- sin(x) * 50 + 50

plot(x, y, type = "l")

chart18   <- as.data.frame(x)
chart18$y <- y

p <- ggplot(chart18, aes(x = x, y = y)) +
  geom_line(color = "darkgreen", size = 1.5) +
  labs(x = "Hour of the Day", y = "Income ($)") +
  theme_supply_demand()

p <- p +
  theme(plot.background = element_rect(color = "black", 
                                       size  = 2, 
                                       fill  = NA),
        plot.margin  = margin(1, 1, 1, 1, "cm"),
        axis.text.y  = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", angle = 90))

plot_title <- textGrob("Chart 21: Caricaturist Income in New York City", 
                       gp = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(p), heights = c(1, 10))

ggsave("plots/chart21.png", final_plot, 
       width  = 10, 
       height = 11, 
       units  = "in", 
       dpi    = 300)


# Chart 22 ----------------------------------------------------------------
source("tax_graph3.R")

demand <- function(Q) 20 - 0.5 * Q
pmc    <- function(Q) 2 + 0.25 * Q
smc    <- function(Q) supply(Q) + 8

DWL <- tax_graph3(demand, pmc, smc) 
DWL$p

plot_title <- textGrob("Chart 22: Neg. Externality & Pigouvian Tax", 
                       gp = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(DWL$p), heights = c(1, 10))

ggsave("plots/chart22.png", final_plot, 
       width  = 10, 
       height = 11, 
       units  = "in", 
       dpi    = 300)

