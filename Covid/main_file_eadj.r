############################# SMCS - Case Study 1 #############################
##################### Application to the Covid-Hub Dataset ####################

################ ----------- E-Process Adjustment ----------- #################

library(dplyr)
library(ggplot2)

load("forecasts_death.rda") # Load forecasts and true values of deaths
load("truth.rda")

source("preprocessing.R") # Load data preprocessing
source("covid_function_eadj.R")

# Design the test for the strong hypothesis. #

sample_size_01 <- covid_quantile_eadj(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.1,
                                 alpha = 0.1) # Perform the test for the 0.1-quantile. #

sample_size_03 <- covid_quantile_eadj(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.3,
                                 alpha = 0.1) # Perform the test for the 0.3-quantile. #

sample_size_05 <- covid_quantile_eadj(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.5,
                                 alpha = 0.1) # Perform the test for the 0.5-quantile. #

sample_size_07 <- covid_quantile_eadj(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.7,
                                 alpha = 0.1) # Perform the test for the 0.7-quantile. #

sample_size_09 <- covid_quantile_eadj(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.9,
                                 alpha = 0.1) # Perform the test for the 0.9-quantile. #

# ------------------------------------------------------------------------------
# GRAPHS

eadj_05 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_05),
  group = "q = 0.5"
)
save(eadj_05, file = "Rda files/eadj_05.rda")

eadj_01 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_01),
  group = "q = 0.1"
)
save(eadj_01, file = "Rda files/eadj_01.rda")

eadj_09 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_09),
  group = "q = 0.9"
)
save(eadj_09, file = "Rda files/eadj_09.rda")

eadj_07 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_07),
  group = "q = 0.7"
)
save(eadj_07, file = "Rda files/eadj_07.rda")

eadj_03 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_03),
  group = "q = 0.3"
)
save(eadj_03, file = "Rda files/eadj_03.rda")

expression(alpha)
combined_data <- rbind(transform(eadj_01),
                       transform(eadj_03),
                       transform(eadj_05),
                       transform(eadj_07))

# Bar plot
library(extrafont)

ggplot(combined_data, aes(x = as.factor(name), y = value, fill = as.factor(name))) + 
  geom_bar(stat = "identity", width = 0.4) +
  facet_wrap(~group) +
  coord_flip() +
  ggtitle("COVID-19 SMCS") +
  ylab("Number of Weeks") +
  xlab("Model") +
  theme_grey(base_size = 18) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.02, linetype = 'solid',
                                        colour = "gray"), 
        panel.grid.minor = element_line(linewidth = 0.02, linetype = 'solid',
                                        colour = "gray"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        strip.text = element_text(size = 17),
        strip.background = element_rect(fill = "white", colour = NA)) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        strip.text = element_text(hjust = 0)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 17),
        text = element_text(family = "Arial")) +
  guides(fill = guide_legend(ncol = 3, title = NULL, title.theme = element_text(size = 17)))  # Adjust the number of columns in the legend

ggsave("Plots/covid_eadj.pdf", width = 14, height = 8.5, device = cairo_pdf)



