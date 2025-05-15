############################# SMCS - Case Study 1 #############################
##################### Application to the Covid-Hub Dataset ####################

################ -------------- Visualizations --------------- ################

library(dplyr)

source("data_loading.R") # Load forecasts and true values of deaths
source("preprocessing.R")

# Plot some basic graphs. #
library(ggplot2)

# Specify the desired quantiles. #
selected_quantiles <- c(0.15, 0.5, 0.975)
selected_models <- c("GT-deep", "baseline", "ensemble", "truth")

# Plot the point forecasts in normal and log scale. #

# Filter out all other models. #
filtered_data <- forecasts_truth %>% 
  filter(quantile %in% selected_quantiles, model %in% selected_models)

# Create a new variable for log-transformed values. #
filtered_data$log_value <- log(filtered_data$value)

# Plot deaths in linear scale. #
death_linear <- 
  ggplot(subset(filtered_data, quantile == 0.5), aes(x = target_end_date, y = value, color = model)) +
  geom_line() +
  scale_color_manual(values = c("GT-deep" = "red3", "baseline" = "forestgreen", "ensemble" = "yellow3", "truth" = "black")) +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_line(linetype = 'solid', colour = "gray"),
        panel.grid.minor = element_line(linetype = 'solid', colour = "gray"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
        legend.text = element_text(size = 22),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),  # Increase x-axis title size
        axis.title.y = element_text(size = 22),
        strip.text = element_text(size = 22),
        text = element_text(family = "Palatino"),
        plot.title = element_text(size = 16)) +
  labs(title = "Covid-19 related deaths in linear scale")

# Plot deaths in log scale. #
death_log <- 
  ggplot(subset(filtered_data, quantile == 0.5), aes(x = target_end_date, y = log_value, color = model)) +
  geom_line() +
  scale_color_manual(values = c("GT-deep" = "red3", "baseline" = "forestgreen", "ensemble" = "yellow3", "truth" = "black")) +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_line(linetype = 'solid', colour = "gray"),
        panel.grid.minor = element_line(linetype = 'solid', colour = "gray"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
        legend.text = element_text(size = 22),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),  # Increase x-axis title size
        axis.title.y = element_text(size = 22),
        strip.text = element_text(size = 22),
        text = element_text(family = "Palatino"),
        plot.title = element_text(size = 16)) +
  labs(title = "Covid-19 related deaths in logarithmic scale")

# Plot the quantile forecasts for the selected quantiles. #

# First for tau = 0.15. #
forecasts_015 <- 
  ggplot(subset(filtered_data, quantile == 0.15), aes(x = target_end_date, y = log_value, color = model)) +
  geom_line() +
  scale_color_manual(values = c("GT-deep" = "red3", "baseline" = "forestgreen", "ensemble" = "yellow3")) +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_line(linetype = 'solid', colour = "gray"),
        panel.grid.minor = element_line(linetype = 'solid', colour = "gray"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
        legend.text = element_text(size = 22),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),  # Increase x-axis title size
        axis.title.y = element_text(size = 22),
        strip.text = element_text(size = 22),
        text = element_text(family = "Palatino"),
        plot.title = element_text(size = 16)) +
  labs(title = "Forecasts for \u03C4 = 0.15")

# Now for tau = 0.9. #
forecasts_0975 <- 
  ggplot(subset(filtered_data, quantile == 0.975), aes(x = target_end_date, y = log_value, color = model)) +
  geom_line() +
  scale_color_manual(values = c("GT-deep" = "red3", "baseline" = "forestgreen", "ensemble" = "yellow3")) +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_line(linetype = 'solid', colour = "gray"),
        panel.grid.minor = element_line(linetype = 'solid', colour = "gray"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
        legend.text = element_text(size = 22),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),  # Increase x-axis title size
        axis.title.y = element_text(size = 22),
        strip.text = element_text(size = 22),
        text = element_text(family = "Palatino"),
        plot.title = element_text(size = 16)) +
  labs(title = "Forecasts for \u03C4 = 0.9")

## Combine all subplots in a panel of four. #

library(patchwork) # install.packages("patchwork")
library(cowplot)

combined_plot <- death_linear + death_log + forecasts_015 + forecasts_0975

## Design the legend. #
common_legend <- ggplot(filtered_data, aes(color = factor(model))) +
  geom_point() +
  labs(title = NULL)

# Add the legend to the combined plot. #
combined_plot <- combined_plot + plot_layout(
  plot_layout(guides = "collect"),
  plot_layout(common_legend + theme(legend.position = "bottom")),
  ncol = 2
)

library(gridExtra)