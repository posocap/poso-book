
# Options -----------------------------------------------------------------
set.seed(123)


# Libraries ---------------------------------------------------------------
if (!require(pacman)) { install.packages("pacman"); library(pacman) }  

p_load(devtools, ggplot2, grid, gridExtra, Hmisc, stringr)

install_github("R-CoderDotCom/econocharts")
require(econocharts)


# Create a Directory to Hold the Plots ------------------------------------
dir.create("plots")


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


# Chart 1: Supply ---------------------------------------------------------
supply_data <- supply()$curve

supply_data$y[85:100] <- NA # b/c I want some whitespace

supply_plot <- ggplot(supply_data, aes(x = x, y = y)) +
  geom_line() +
  geom_text(aes(label = "Supply (S)", x = Inf, y = Inf, hjust = 1.1, vjust = 1), color = "blue", size = 8) +
  labs(x = "Quantity", y = "Price ($)") +
  theme_supply_demand() +
  theme(plot.title = element_blank())

plot_title <- textGrob("Chart 1: Supply Curve", gp = gpar(fontface = "bold", fontsize = 30))

final_plot <- grid.arrange(plot_title, supply_plot, heights = c(1, 10))

# Save the plot
ggsave("plots/chart_1.png", final_plot, width = 10, height = 11, units = "in", dpi = 300)


# Chart 2 -----------------------------------------------------------------
demand_data <- econocharts::demand()$curve
demand_plot <- ggplot(demand_data, aes(x = x, y = y)) +
  geom_line(color = "red") +
  geom_text(aes(label = "Demand (D)", x = max(x)-1, y = min(y)+1), color = "red", size = 6) +
  labs(x = "Quantity", y = "Price ($)") +
  theme_supply_demand()
demand_plot

plot_title <- textGrob("Chart 2: Demand Curve", gp = gpar(fontface = "bold", fontsize = 32))

final_demand_plot <- grid.arrange(plot_title, demand_plot, heights = c(1, 10))
final_demand_plot
ggsave("plots/chart_2.png", final_demand_plot, width = 10, height = 11, units = "in", dpi = 300)


# Chart 3 -----------------------------------------------------------------
sd_plot <- sdcurve(generic = TRUE, equilibrium = F)
sd_plot <- sd_plot +
  labs(x = "Quantity", y = "Price ($)") +
  theme_supply_demand()

plot_title <- textGrob("Chart 3: Supply and Demand", gp = gpar(fontface = "bold", fontsize = 32))
final_sd_plot <- grid.arrange(plot_title, sd_plot, heights = c(1, 10))

ggsave("plots/chart_3.png", final_sd_plot, width = 10, height = 11, units = "in", dpi = 300)


# Chart 4 -----------------------------------------------------------------
# Chart A
equilibrium_plot <- sdcurve(supply_data, demand_data, equilibrium = TRUE) +
  ggplot2::geom_segment(data = equilibrium_point, aes(x = x, xend = x, y = 0, yend = y), 
                        linetype = "dotted", color = "darkgreen") +
  ggplot2::geom_segment(data = equilibrium_point, aes(x = 0, xend = x, y = y, yend = y), 
                        linetype = "dotted", color = "darkgreen") +
  labs(x = "Quantity", y = "Price ($)") +
  theme_supply_demand()

plot_title       <- textGrob("Chart 4: Equilibrium", gp = gpar(fontface = "bold", fontsize = 32))
equilibrium_plot <- grid.arrange(plot_title, equilibrium_plot, heights = c(1, 10))

ggsave("plots/chart_4.png", equilibrium_plot, width = 10, height = 10, units = "in", dpi = 300)

# Chart B
supply1 <- data.frame(x = c(1, 9), y = c(1, 9))
supply1

demand1 <- data.frame(x = c(7, 2), y = c(2, 7))
demand1

supply2 <- data.frame(x = c(2, 10), y = c(1, 9))
supply2

demand2 <- data.frame(x = c(8, 2), y = c(2, 8))
demand2

p <- sdcurve(supply1,
             demand1,
             supply1, 
             demand2,
             equilibrium = TRUE)
p + annotate("segment", x = 2.5, xend = 3, y = 6.5, yend = 7,
             arrow = arrow(length = unit(0.3, "lines")), colour = "grey50")

# Chart C
p <- sdcurve(supply1,
             demand1,
             supply2, 
             demand1,
             equilibrium = TRUE)
p + annotate("segment", x = 2.5, xend = 3, y = 6.5, yend = 7,
             arrow = arrow(length = unit(0.3, "lines")), colour = "grey50")


# Chart 5 -----------------------------------------------------------------
# Supply curve
supply_curve <- econocharts::supply()$curve

# Plotting hack - fix shading by flattening curves
demand_curve <- data.frame(
  x = seq(from = min(econocharts::demand()$curve$x), 
          to   = max(econocharts::demand()$curve$x), 
          length.out = length(econocharts::demand()$curve$x)),
  y = seq(from = 9, to = 1, length.out = length(econocharts::demand()$curve$y))
)
supply_curve <- data.frame(
  x = seq(from = min(econocharts::demand()$curve$x), 
          to   = max(econocharts::demand()$curve$x), 
          length.out = length(econocharts::demand()$curve$x)),
  y = seq(from = 1, to = 9, length.out = length(econocharts::demand()$curve$y))
)

# Key points
price_ceiling_value <- 2.5
price_ceiling_x     <- demand_curve$x[which(demand_curve$y <= price_ceiling_value)[1]]
price_ceiling_xmax  <- tail(demand_curve$x, 1)

# Base plot
sd_plot <- econocharts::sdcurve(supply_curve, demand_curve, 
                                max.price = price_ceiling_value, generic = T, 
                                xlab = "Quantity", ylab = "Price") +
  theme_supply_demand()

# Formatted plot
x_intercept  <- demand_curve$x[which.min(abs(demand_curve$y - price_ceiling_value))]
x1_intercept <- supply_curve$x[which.max(supply_curve$x[supply_curve$y <= 2.5])]
y1_intercept <- demand_curve$y[which.min(abs(demand_curve$x - x1_intercept))]
equilibrium  <- curve_intersect(supply_curve, demand_curve)
  
disequilibrium_plot <- sd_plot +
                      annotate("segment", x = x1_intercept, xend = x1_intercept,
                               y = 0, yend = y1_intercept,
                               linetype = "dotted") +
                      annotate("segment", x = x_intercept, xend = x_intercept,
                               y = 0, yend = price_ceiling_value,
                               linetype = "dotted") + 
                      geom_polygon(data = data.frame(x = c(1, x1_intercept, 1),
                                 y = c(min(supply_data$x), 2.454545, x1_intercept)),
                                  aes(x = x, y = y), fill = "green", alpha = 0.2) +
                      geom_polygon(data = data.frame(x = c(1, equilibrium$x, 1),
                                                     y = c(max(demand_data$y), 
                                                           equilibrium$y, 
                                                           equilibrium$y)),
                                   aes(x = x, y = y), 
                                   fill = "yellow", 
                                   alpha = 0.2) +
                      geom_polygon(data = data.frame(x = c(1, x1_intercept, x1_intercept, 1), # Extra consumer surplus
                                                     y = c(price_ceiling + 0.25, 
                                                           price_ceiling + 0.2, 
                                                           equilibrium$y, 
                                                           equilibrium$y)),
                                   aes(x = x, y = y), fill = "yellow", alpha = 0.2) +
                      geom_polygon(data = data.frame(x = c(x1_intercept, 
                                                           equilibrium$x, 
                                                           x1_intercept),
                                                     y = c(supply_curve$y[
                                                       which.max(
                                                         supply_curve$x >= 
                                                           x1_intercept)], 
                                                           equilibrium$y, 
                                                           equilibrium$y)),
                                   aes(x = x, y = y), 
                                   fill = "red", 
                                   alpha = 0.5) +
                      geom_polygon(data = data.frame(x = c(x1_intercept, 
                                                           equilibrium$x, 
                                                           x1_intercept),
                                                     y = c(
                                                       y1_intercept, 
                                                       equilibrium$y, 
                                                       equilibrium$y)),
                                   aes(x = x, y = y), 
                                   fill = "red", 
                                   alpha = 0.5)


disequilibrium_plot

disequilibrium_plot <- disequilibrium_plot +
  annotate("text", label = "A", x = x1_intercept, y = price_ceiling_value, 
           vjust = 1.5) +
  annotate("text", label = "B", x = x_intercept, y = price_ceiling_value, 
           vjust = 1.5) +
  annotate("text", label = "Qs", x = x1_intercept, y = 0, vjust = -.65, hjust = .25) +
  annotate("text", label = "Qd", x = x_intercept, y = 0, vjust = -.65, hjust = .25) + 
  annotate("text", 
           label = "Ceiling", x = 8.5, 
           y = price_ceiling_value, 
           vjust = -0.5)

plot_title <- textGrob("Chart 5: Shortage from a Price Ceiling", 
                       gp = gpar(fontface = "bold", fontsize = 32))

final_demand_plot <- grid.arrange(plot_title, disequilibrium_plot, heights = c(1, 10))

ggsave("plots/chart_5.png", 
       final_demand_plot, 
       width  = 10, 
       height = 11, 
       units  = "in", 
       dpi    = 300)


# Chart 6 -----------------------------------------------------------------
utility_data <- data.frame(
  quantity = 1:10,
  total_utility    = c(10, 18, 24, 28, 30, 31, 31.5, 32, 32.25, 32.4),
  marginal_utility = c(10, 8, 6, 4, 2, 1, 0.5, 0.5, 0.25, 0.15)
)
utility_data$total_utility    <- loess(total_utility ~    quantity, data = utility_data)$fitted
utility_data$marginal_utility <- loess(marginal_utility ~ quantity, data = utility_data)$fitted

utility_plot <- ggplot(utility_data, aes(x = quantity)) +
  geom_line(aes(y = total_utility), color = "blue") +
  geom_line(aes(y = marginal_utility), color = "red") +
  labs(x = "Quantity", y = "Utility") +
  theme_supply_demand()

# Wrap the title and create the textGrob
wrapped_title <- str_wrap("Chart 6: Marginal and Total Utility", width = 80)
plot_title    <- textGrob(wrapped_title, gp = gpar(fontface = "bold", fontsize = 32))

# Combine the title and plot
final_utility_plot <- grid.arrange(plot_title, utility_plot, ncol = 1, nrow = 2, heights = c(1, 10))

# Save the plot
ggsave("plots/chart_6.png", final_utility_plot, 
       width = 10, height = 11, units = "in", dpi = 300)


# Chart 7 -----------------------------------------------------------------
p <- indifference(ncurves = 2, x = c(2, 4), main = "", xlab = "Good X", ylab = "Good Y")

p$p <- p$p + theme(plot.background = element_rect(color = "black", size = 2, fill = NA),
                   plot.margin = margin(1, 1, 1, 1, "cm"))

int <- bind_rows(curve_intersect(data.frame(x = 1:1000, y = rep(3, 1000)), p$curve + 1))

enhanced_plot <- p$p + 
  geom_segment(data = int, aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = int, aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_point(data = int, aes(x = x, y = y), size = 3)

enhanced_plot <- enhanced_plot +
  geom_text(data = int, aes(x = x, y = y, label = "C"),
            vjust = -1, hjust = -0.5, color = "black", size = 4)


plot_title <- textGrob("Chart 7: Indifference Curves", 
                       gp = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(enhanced_plot), heights = c(1, 10))

ggsave("plots/chart_7.png", final_plot, width = 10, height = 11, units = "in", dpi = 300)


# Chart 8 -----------------------------------------------------------------
p <- indifference(ncurves = 3, main = "", 
                  xlab = "Good X", ylab = "Good Y",
                  type = "pcom", generic = T)

p$p <- p$p + theme(plot.background = element_rect(color = "black", size = 2, fill = NA),
                   plot.margin = margin(1, 1, 1, 1, "cm"))

plot_title <- textGrob("Chart 8: Indiff. Curves - Perfect Complements", 
                       gp = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(p$p), heights = c(1, 10))

ggsave("plots/chart_8.png", final_plot, 
       width = 10, 
       height = 11, 
       units = "in", 
       dpi = 300)


# Chart 9 -----------------------------------------------------------------
p <- indifference(ncurves = 3, 
                  main    = "", 
                  xlab    = "Good X", 
                  ylab    = "Good Y",
                  type    = "psubs", 
                  generic = T)

p$p <- p$p + theme(plot.background = element_rect(color = "black", 
                                                  size  = 2, 
                                                  fill  = NA),
                   plot.margin = margin(1, 1, 1, 1, "cm"))

plot_title <- textGrob("Chart 9: Indiff. Curves - Perfect Substitutes", 
                       gp = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(p$p), heights = c(1, 10))

ggsave("plots/chart_9.png", final_plot, 
       width = 10, 
       height = 11, 
       units = "in", 
       dpi = 300)


# Chart 10 ----------------------------------------------------------------
p <- ppf(x = 4:6,
         geom    = "label",
         generic = TRUE,
         labels  = c("A", "B", "C"),
         xlab    = "Diet Pills (quantity)",
         ylab    = "Cognitive Enhancers (quantity)",
         acol    = 3)

p$p <- p$p + geom_point(data = data.frame(x = 5, y = 5), size = 3) +
  geom_point(data = data.frame(x = 2, y = 2), size = 3) +
  annotate("text", label = "D", x = 2, y = 2) +
  annotate("segment", x = 3.1, xend = 4.25, y = 5, yend = 5,
           arrow = arrow(length = unit(0.5, "lines")), colour = 3, lwd = 1) +
  annotate("segment", x = 4.25, xend = 4.25, y = 5, yend = 4,
           arrow = arrow(length = unit(0.5, "lines")), colour = 2, lwd = 1) +
  theme(plot.background = element_rect(color = "black", 
                                       size  = 2, 
                                       fill  = NA),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", angle = 90))

plot_title <- textGrob("Chart 10: Production Possibilities Frontier", 
                       gp = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(p$p), heights = c(1, 10))

ggsave("plots/chart_10.png", final_plot, 
       width = 10, 
       height = 11, 
       units = "in", 
       dpi = 300)

# Chart 11 ----------------------------------------------------------------
demand <- function(Q) 20 - 0.5 * Q
supply <- function(Q) 2 + 0.25 * Q
supply_tax <- function(Q) supply(Q) + 4

chart11 <- tax_graph(demand, 
                     supply, 
                     supply_tax, 
                     NULL, 
                     xlab = "Quantity (Q)")

chart11$p <- chart11$p +
  theme(plot.background = element_rect(color = "black", 
                                       size  = 2, 
                                       fill  = NA),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", angle = 90))

plot_title <- textGrob("Chart 11: Effect of Tax on Equilibrium", 
                       gp = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(chart11$p), heights = c(1, 10))

ggsave("plots/chart_11.png", final_plot, 
       width = 10, 
       height = 11, 
       units = "in", 
       dpi = 300)


# Chart 12 ----------------------------------------------------------------
chart12 <- tax_graph(demand, supply, supply_tax, 
                     shaded = TRUE,xlab = "Quantity (Q)")

chart12$p <- chart12$p +
  theme(plot.background = element_rect(color = "black", 
                                       size  = 2, 
                                       fill  = NA),
        plot.margin  = margin(1, 1, 1, 1, "cm"),
        axis.text.y  = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", angle = 90))

plot_title <- textGrob("Chart 12: Effect of Tax on Surplus", 
                       gp   = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(chart12$p), heights = c(1, 10))

ggsave("plots/chart_12.png", final_plot, 
       width  = 10, 
       height = 11, 
       units  = "in", 
       dpi    = 300)

# Chart 13 ----------------------------------------------------------------
cross_price_data <- data.frame(
  price_of_good_x = 1:100,
  demand_for_good_y = 100 - log(1:100)
)

cross_price_plot <- ggplot(cross_price_data, aes(x = price_of_good_x, y = demand_for_good_y)) +
  geom_line(color = "purple", size = 1.5) +
  labs(title = "Chart 13: Cross-price Elasticity of Demand", x = "Price of Good X ($)", y = "Demand for Good Y") +
  theme_supply_demand()

plot_title <- textGrob("Chart 13: Cross-price Elasticity of Demand", 
                       gp   = gpar(fontface = "bold", fontsize = 32),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(cross_price_plot), heights = c(1, 10))

print(final_plot)

ggsave("plots/chart_13.png", final_plot, width = 10, height = 10, units = "in", dpi = 300)


# Chart 14 ----------------------------------------------------------------
market_data <- data.frame(
  Quantity     = 1:100,
  Demand_Price = 100:1,
  Supply_Price = 1:100,
  Supply_Price2 = c(NA, NA, 1:98 + 20)  # Start the second supply curve at a higher index to prevent extending the y-range
)

firm_data <- data.frame(
  Quantity = 1:100,
  Price    = rep(50, 100),
  Price2   = rep(60, 100)  # Second horizontal demand curve at 70
)

# Determine the consistent y-limits for both plots
y_limits <- range(0, market_data$Demand_Price, market_data$Supply_Price, firm_data$Price, firm_data$Price2, na.rm = TRUE)

# Market plot with additional supply curve
market_plot <- ggplot(market_data, aes(x = Quantity)) +
  geom_line(aes(y = Demand_Price, color = "Demand"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = Supply_Price, color = "Supply"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = Supply_Price2, color = "Supply 2"), linetype = "dashed", size = 1.2, color = "purple") +
  labs(title = "Industry Level", x = "Quantity", y = "Price ($)") +
  scale_color_manual(values = c(Demand = "blue", Supply = "red", "Supply 2" = "purple")) +
  scale_y_continuous(limits = y_limits) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(size = 18))

# Firm plot with an additional horizontal demand curve
firm_plot <- ggplot(firm_data, aes(x = Quantity)) +
  geom_line(aes(y = Price), color = "blue") +
  geom_line(aes(y = Price2), color = "blue", linetype = "dashed") +
  labs(title = "Firm Level", x = "Quantity", y = "Price ($)") +
  scale_y_continuous(limits = y_limits) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18))

# Combine the plots
combined_plot <- grid.arrange(
  grobs = list(market_plot, firm_plot),
  ncol = 2
)

grid.newpage()
grid.draw(combined_plot)


border_grob <- rectGrob(
  x = 0.5, y = 0.5, width = 1, height = 1,
  just = c("center", "center"),
  gp = gpar(col = "black", lwd = 2, fill = NA)
)

# Note: For now, I'm going to use the red dotted line in Word instead of R
#           because it's easier to align
grid.newpage()
grid.draw(combined_plot)
grid.draw(line_grob) 
grid.draw(border_grob)

g <- grid.grab()

plot_title <- textGrob("Chart 14: Perfect Competition (Short Run)", 
                       gp = gpar(fontface = "bold", fontsize = 30))

final_plot <- grid.arrange(plot_title, g, heights = c(1, 10))

ggsave("plots/combined_chart_14.png", final_plot, 
       width = 10, height = 11, units = "in", dpi = 300)


# Chart 15 ----------------------------------------------------------------
x_values           <- seq(0, 8, length.out = 300) * 10
y_values           <- spline(x = c(1, 2, 3, 4, 5, 6, 7) * 10, y = c(2.5, 1.5, 3, 5, 7, 9, 11) * 10, xout = x_values)$y
custom_mc          <- as.data.frame(lowess(x = x_values, y = y_values, f = 0.1, iter = 0))
colnames(custom_mc) <- c("x", "y")

market_data        <- data.frame(Quantity = 1:100, Demand_Price = 100:1, Supply_Price = 1:100)
firm_data          <- data.frame(Quantity = 1:100, Price = rep(50, 100))

custom_mc_rescaled <- approx(custom_mc$x, custom_mc$y, xout = firm_data$Quantity)$y
firm_data$MC       <- custom_mc_rescaled
firm_data$MC[1:10] <- NA
firm_data$MC[firm_data$MC > 70] <- NA

intersect_index    <- which.min(abs(firm_data$MC - firm_data$Price))
intersect_point    <- firm_data[intersect_index, ]

market_plot        <- ggplot(market_data, aes(x = Quantity)) +
  geom_line(aes(y = Demand_Price, color = "Demand"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = Supply_Price, color = "Supply"), linetype = "solid", size = 1.2) +
  geom_text(x = 25, y = 80, label = "D", color = "blue", hjust = 0.6) +
  geom_text(x = 75, y = 80, label = "S", color = "red", hjust = 0.6) +
  annotate("segment", x = 40, xend = 40, y = 0, yend = 50, linetype = "dotted", color = "black") +
  labs(title = "Industry Level", x = "Quantity", y = "Price ($)") +
  scale_color_manual(values = c(Demand = "blue", Supply = "red")) +
  scale_y_continuous(limits = y_limits) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(size = 18))

a <- 0.0329
h <- 40
k <- 50

x_start <- 40 - 0.75 * 40
x_end   <- 40 + 0.75 * 40

firm_plot <- ggplot(firm_data, aes(x = Quantity)) +
  geom_line(aes(y = Price), color = "blue") +
  geom_line(aes(y = MC, color = "Marginal Costs"), color = "darkgreen", 
            size = 1.1) +
  geom_text(x = 47, y = 65, label = "MC", color = "darkgreen", hjust = -0.5) +
  stat_function(fun = function(x) ifelse(x > x_start & x < x_end, a*(x - h)^2 + k, NA), color = "red") +
  geom_text(x = x_end, y = a*(x_end - h)^2 + k, label = "ATC", color = "red", 
            hjust = -0.5, vjust = -1.1) +
  annotate("segment", x = 40, xend = 40, y = 0, yend = 50, linetype = "dotted", color = "black") +
  labs(title = "Firm Level", x = "Quantity", y = "Price ($)") +
  scale_color_manual(values = c("Price" = "blue", "Marginal Costs" = "darkgreen")) +
  scale_y_continuous(limits = y_limits) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18))

combined_plot <- grid.arrange(market_plot, firm_plot, ncol = 2)
grid.newpage()
grid.draw(combined_plot)

border_grob <- rectGrob(
  x = 0.5, y = 0.5, width = 1, height = 1,
  just = c("center", "center"),
  gp = gpar(col = "black", lwd = 2, fill = NA)
)
grid.draw(border_grob)
plot_title <- textGrob("Chart 15: Perfect Competition (Long Run)", 
                       gp = gpar(fontface = "bold", fontsize = 30))

g          <- grid.grab()
final_plot <- grid.arrange(plot_title, g, heights = c(1, 10))

ggsave("plots/combined_chart_15.png", final_plot, width = 10, height = 11, units = "in", dpi = 300)


# Chart 16 ----------------------------------------------------------------
demand     <- function(Q) 20 - 0.5 * Q
supply     <- function(Q) 2 + 0.25 * Q
supply_tax <- function(Q) supply(Q) + 4

source(file = "tax_graph2.R")
chart16 <- tax_graph2(demand, supply, supply_tax, 
                     shaded = TRUE, xlab = "Quantity (Q)")

chart16$p$layers[[1]]  <- NULL
chart16$p$layers[[3]]  <- NULL
chart16$p$layers[[4]]  <- NULL
chart16$p$layers[[5]]  <- NULL
chart16$p$layers[[6]]  <- NULL
chart16$p$layers[[1]]  <- NULL
chart16$p$layers[[3]]  <- NULL
chart16$p$layers[[2]]  <- NULL
chart16$p

p <- chart16$p
print(p)

p <- p +
  theme(plot.background = element_rect(color = "black", 
                                       size  = 2, 
                                       fill  = NA),
        plot.margin  = margin(1, 1, 1, 1, "cm"),
        axis.text.y  = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", angle = 90))

plot_title <- textGrob("Chart 16: Welfare Loss Resulting from Monopoly", 
                       gp   = gpar(fontface = "bold", fontsize = 30),
                       just = "center")

final_plot <- grid.arrange(plot_title, ggplotGrob(p), heights = c(1, 10))

ggsave("plots/chart_16.png", final_plot, 
       width  = 10, 
       height = 11, 
       units  = "in", 
       dpi    = 300)


# Chart 17 ----------------------------------------------------------------
x_values            <- seq(0, 8, length.out = 300) * 10
y_values            <- spline(x = c(1, 2, 3, 4, 5, 6, 7) * 10, y = c(2.5, 1.5, 3, 5, 7, 9, 11) * 10, xout = x_values)$y
custom_mc           <- as.data.frame(lowess(x = x_values, y = y_values, f = 0.1, iter = 0))
colnames(custom_mc) <- c("x", "y")

market_data         <- data.frame(Quantity     = 1:100, 
                                  Demand_Price = 100:1, 
                                  Supply_Price = 1:100)

firm_data           <- data.frame(Quantity = 1:100, Price = rep(50, 100))

custom_mc_rescaled  <- approx(custom_mc$x, custom_mc$y, xout = firm_data$Quantity)$y
firm_data$MC        <- custom_mc_rescaled
firm_data$MC[1:10]  <- NA
firm_data$MC[firm_data$MC > 70] <- NA

intersect_index    <- which.min(abs(firm_data$MC - firm_data$Price))
intersect_point    <- firm_data[intersect_index, ]

market_plot        <- ggplot(market_data, aes(x = Quantity)) +
  geom_line(aes(y = Demand_Price, color = "Demand"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = Supply_Price, color = "Supply"), linetype = "solid", size = 1.2) +
  geom_text(x = 25, y = 80, label = "D", color = "blue", hjust = 0.6) +
  geom_text(x = 75, y = 80, label = "S = MC", color = "red", hjust = 0.9) +
  geom_text(x = 27, y = 25, label = "MR", color = "darkgreen", hjust = -0.5, vjust = 2) +
  annotate("segment", x = 0, xend = 30, y = 100, yend = 25, color = "darkgreen") +
  
  labs(title = "Industry Level", x = "Quantity", y = "Price ($)") +
  scale_color_manual(values = c(Demand = "blue", Supply = "red")) +
  scale_y_continuous(limits = y_limits) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(size = 18))

a <- 0.06
h <- 40
k <- 50

x_start <- 40 - 0.75 * 40
x_end   <- 40 + 0.75 * 40

firm_plot <- ggplot(firm_data, aes(x = Quantity)) +
  geom_line(aes(y = Price), color = "blue") +
  geom_line(aes(y = MC, color = "Marginal Costs"), color = "darkgreen", 
            size = 1.1) +
  geom_text(x = 47, y = 65, label = "MC", color = "darkgreen", hjust = -0.5) +
  stat_function(fun = function(x) ifelse(x > x_start & x < x_end, a*(x - h)^2 + k, NA), color = "red") +
  geom_text(x = x_end, y = 0.03*(x_end - h)^2 + k, label = "ATC", color = "red", 
            hjust = -0.5, vjust = -1.1) +
  geom_text(x = 2, y = 52, label = "D", color = "blue", hjust = 0.6) +
  labs(title = "Firm Level", x = "Quantity", y = "Price ($)") +
  scale_color_manual(values = c("Price" = "blue", "Marginal Costs" = "darkgreen")) +
  scale_y_continuous(limits = y_limits) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18))

combined_plot <- grid.arrange(market_plot, firm_plot, ncol = 2)
grid.newpage()
grid.draw(combined_plot)

border_grob <- rectGrob(
  x = 0.5, y = 0.5, width = 1, height = 1,
  just = c("center", "center"),
  gp = gpar(col = "black", lwd = 2, fill = NA)
)
grid.draw(border_grob)
plot_title <- textGrob("Chart 17: Cartel Pricing via Oligopoly", 
                       gp = gpar(fontface = "bold", fontsize = 30))

g          <- grid.grab()
final_plot <- grid.arrange(plot_title, g, heights = c(1, 10))

ggsave("plots/combined_chart_17.png", final_plot, width = 10, height = 11, units = "in", dpi = 300)

