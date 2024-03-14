### SMCS ###
### Application to the CovidHub dataset ###


#install dependencies and packages from the github page https://github.com/reichlab/covid19-forecast-hub
#install.packages("remotes")
#install.packages("devtools")

library(remotes)
library(devtools)
library(dplyr)

setwd("/Users/ggavrilopoul/polybox - Georgios Gavrilopoulos (georgios.gavrilopoulos@stat.math.ethz.ch)@polybox.ethz.ch/PhD/EMCS/RCode/Case Study: Covid19 cases")

#remotes::install_github('reichlab/zoltr')
#remotes::install_github('epiforecasts/scoringutils')
#devtools::install_github('reichlab/covidHubUtils')
#remotes::install_github("Chicago/RSocrata")
#install.packages(c('tidyverse', 'rvest', 'here', 'covidcast'))
#remotes::install_github("reichlab/covidData")

library(covidData)
library(covidHubUtils)

# Download forecasts for the US

forecasts_death <- load_forecasts(
  models = NULL,
  date_window_size = 0,
  locations = "US",
  hub = "US",
  types = c("quantile"),
  targets = paste(1, "wk ahead inc death"),
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL
)

# Download true statistics (cases/deaths/hospitalizations)

truth <- load_truth(
  truth_source = "JHU",
  hub = "US",
  locations = "US",
  target_variable = c("inc death"),
  temporal_resolution = "weekly"
)

# if you want to turn back
# forecasts_hosp = forecasts_sos

# now remove the unnecessary columns
forecasts_death <- forecasts_death[,c("model", "forecast_date", "target_end_date", "quantile", "value")]

# remove rows with NAs - in any case the point forecasts is always the median
forecasts_death <- na.omit(forecasts_death)

# keep only some of the quantiles, namely 0.05, 0.25, 0.5, 0.75, 0.95
# forecasts_death <- forecasts_death[forecasts_death$quantile %in% c(0.05, 0.25, 0.5, 0.75, 0.95), ]

################### END OF DATA LOADING ##############

################### DATA CLEANING ####################

library(dplyr)

forecasts_death$model <- as.factor(forecasts_death$model) # transform "model" to a factor

# get overview
table(forecasts_death$model)

#remove models with very few predictions (<1800)
forecasts_death <- forecasts_death[!(as.numeric(forecasts_death$model) %in% which(table(forecasts_death$model)<1800)),]

# drop the corresponding levels from the factor "model"
forecasts_death$model <- droplevels(forecasts_death$model)

#keep only the models that align with the timeline of the true data
forecasts_death <- forecasts_death[forecasts_death$target_end_date %in% truth$target_end_date, ]

# again remove models with few predictions (<1800)
forecasts_death <- forecasts_death[!(as.numeric(forecasts_death$model) %in% which(table(forecasts_death$model)<1800)),]

# keep the common dates between all models
#reference_level <- counts_model[counts_model[,2]==min(counts_model[,2]),1]$model # model with the fewest values
#intersect_dates <- unique(forecasts_death[forecasts_death$model == reference_level,]$target_end_date)
intersect_end_dates <- as.Date(Reduce(intersect,list(unique(forecasts_death[forecasts_death$model=="COVIDhub-ensemble",]$target_end_date),
                                         #unique(forecasts_death[forecasts_death$model=="CU-select",]$target_end_date),
                                         unique(forecasts_death[forecasts_death$model=="PSI-DRAFT",]$target_end_date),
                                         #unique(forecasts_death[forecasts_death$model=="CU-nochange",]$target_end_date),
                                         #unique(forecasts_death[forecasts_death$model=="CU-scenario_low",]$target_end_date),
                                         unique(forecasts_death[forecasts_death$model=="COVIDhub_CDC-ensemble",]$target_end_date),
                                         unique(forecasts_death[forecasts_death$model=="COVIDhub-4_week_ensemble",]$target_end_date),
                                         #unique(forecasts_death[forecasts_death$model=="UCSD_NEU-DeepGLEAM",]$target_end_date),
                                         unique(forecasts_death[forecasts_death$model=="COVIDhub-baseline",]$target_end_date),
                                         unique(forecasts_death[forecasts_death$model=="GT-DeepCOVID",]$target_end_date),
                                         #unique(forecasts_death[forecasts_death$model=="Karlen-pypm",]$target_end_date),
                                         unique(forecasts_death[forecasts_death$model=="MOBS-GLEAM_COVID",]$target_end_date)
                                         #unique(forecasts_death[forecasts_death$model=="RobertWalraven-ESG",]$target_end_date),
                                         #unique(forecasts_death[forecasts_death$model=="BPagano-RtDriven",]$target_end_date),
                                         #unique(forecasts_death[forecasts_death$model=="YYG-ParamSearch",]$target_end_date),
                                         #unique(forecasts_death[forecasts_death$model=="CU-scenario_mid",]$target_end_date)
                                         )))

intersect_forecast_dates <- as.Date(Reduce(intersect,list(unique(forecasts_death[forecasts_death$model=="COVIDhub-ensemble",]$forecast_date),
                                                          #unique(forecasts_death[forecasts_death$model=="CU-select",]$forecast_date),
                                                          unique(forecasts_death[forecasts_death$model=="PSI-DRAFT",]$forecast_date),
                                                          #unique(forecasts_death[forecasts_death$model=="CU-nochange",]$forecast_date),
                                                          #unique(forecasts_death[forecasts_death$model=="CU-scenario_low",]$forecast_date),
                                                          unique(forecasts_death[forecasts_death$model=="COVIDhub_CDC-ensemble",]$forecast_date),
                                                          #unique(forecasts_death[forecasts_death$model=="COVIDhub-4_week_ensemble",]$forecast_date),
                                                          #unique(forecasts_death[forecasts_death$model=="UCSD_NEU-DeepGLEAM",]$forecast_date),
                                                          unique(forecasts_death[forecasts_death$model=="COVIDhub-baseline",]$forecast_date),
                                                          unique(forecasts_death[forecasts_death$model=="GT-DeepCOVID",]$forecast_date),
                                                          #unique(forecasts_death[forecasts_death$model=="Karlen-pypm",]$forecast_date),
                                                          unique(forecasts_death[forecasts_death$model=="MOBS-GLEAM_COVID",]$forecast_date)
                                                          #unique(forecasts_death[forecasts_death$model=="RobertWalraven-ESG",]$forecast_date),
                                                          #unique(forecasts_death[forecasts_death$model=="BPagano-RtDriven",]$forecast_date),
                                                          #unique(forecasts_death[forecasts_death$model=="YYG-ParamSearch",]$forecast_date),
                                                          #unique(forecasts_death[forecasts_death$model=="CU-scenario_mid",]$forecast_date)
)))


forecasts_target <- forecasts_death[(forecasts_death$target_end_date %in% intersect_end_dates) & (forecasts_death$forecast_date %in% intersect_forecast_dates), ]

# there are some duplicate rows (multiple predictions for the same date): we remove them
forecasts_target <- forecasts_target[!(duplicated(forecasts_target[,c(1,3,4)])),]

# again remove models with few predictions (<1800)
forecasts_target <- forecasts_target[!(as.numeric(forecasts_target$model) %in% which(table(forecasts_target$model)<1800)),]
forecasts_target$model <- droplevels(forecasts_target$model)

# remove some models with non conforming dates
forecasts_target <- forecasts_target[forecasts_target$model %in% c("COVIDhub-ensemble", "PSI-DRAFT", "COVIDhub-baseline", "GT-DeepCOVID",
                                                                   "MOBS-GLEAM_COVID", "COVIDhub_CDC-ensemble"), ]

# forecasts_target <- forecasts_target[forecasts_target$model %in% c("COVIDhub-ensemble", "COVIDhub_CDC-ensemble", "COVIDhub-baseline", "GT-DeepCOVID", "MOBS-GLEAM_COVID", "CU-scenario_mid"), ]

forecasts_target$model <- droplevels(forecasts_target$model)

# discard irrelevant dates from "truth"
truth <- truth[truth$target_end_date %in% unique(forecasts_target$target_end_date), ]

# discard irrelevant columns from "truth"
truth <- truth[,c("model", "target_end_date", "value")]
save(truth, file = "truth.rda")

# we want to merge "truth" with "forecast_target" so we should make some adjustments
missing_cols <- setdiff(names(forecasts_target), names(truth))
truth[missing_cols] <- 0.5
forecasts_truth <- rbind(forecasts_target, truth)

# rename the models
levels(forecasts_truth$model) <- c("CDC_ensemble",
                                   "baseline",
                                   "ensemble",
                                   "GT-deep",
                                   "mobs_gleam",
                                   "psi-draft",
                                   "truth"
                                    )

levels(forecasts_target$model) <- c("CDC_ensemble",
                                   "baseline",
                                   "ensemble",
                                   "GT-deep",
                                   "mobs_gleam",
                                   "psi-draft"
)


save(forecasts_target, file = "forecasts_target.rda") # save dataframe that contains forecasts



#get overview of models and number of forecasts
counts_model <- forecasts_target %>% count(model) # overview of counts of factor "model"

################### END OF DATA CLEANING #############

################### DATA ANALYSIS ####################

# first construct some basic plots
library(ggplot2)

# Specify the desired quantiles
selected_quantiles <- c(0.15, 0.5, 0.975)
selected_models <- c("GT-deep", "baseline", "ensemble", "truth")

## Plot the point forecasts in normal and log scale

## filter out all other models
filtered_data <- forecasts_truth %>% 
  filter(quantile %in% selected_quantiles, model %in% selected_models)

# Create a new variable for log-transformed values
filtered_data$log_value <- log(filtered_data$value)

## Plot deaths in linear scale
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

## Plot deaths in log scale
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

## Plot the quantile forecasts for the selected quantiles

## first for tau = 0.15
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

## Now for tau = 0.9
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

## combine all subplots in a panel of four

library(patchwork) # install.packages("patchwork")
library(cowplot)

combined_plot <- death_linear + death_log + forecasts_015 + forecasts_0975

## design the legend
common_legend <- ggplot(filtered_data, aes(color = factor(model))) +
  geom_point() +
  labs(title = NULL)

# Add the legend to the combined plot
combined_plot <- combined_plot + plot_layout(
  plot_layout(guides = "collect"),
  plot_layout(common_legend + theme(legend.position = "bottom")),
  ncol = 2
)

library(gridExtra)
grid.arrange(death_plot, quantile_forecast_plot, ncol = 2)

## design the test for the strong hypothesis
source("/Users/ggavrilopoul/polybox - Georgios Gavrilopoulos (georgios.gavrilopoulos@stat.math.ethz.ch)@polybox.ethz.ch/PhD/EMCS/RCode/Case Study: Covid19 cases/covid_function.R")


sample_size_01 <- covid_quantile(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.1,
                                 alpha = 0.1) # perform the test for the 0.1-quantile

sample_size_03 <- covid_quantile(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.3,
                                 alpha = 0.1) # perform the test for the 0.3-quantile

sample_size_05 <- covid_quantile(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.5,
                                 alpha = 0.1) # perform the test for the 0.5-quantile

sample_size_07 <- covid_quantile(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.7,
                                 alpha = 0.1) # perform the test for the 0.7-quantile

sample_size_09 <- covid_quantile(forecast_data = forecasts_target,
                                 truth_data = truth,
                                 quantile_value = 0.9,
                                 alpha = 0.1) # perform the test for the 0.9-quantile




################### GRAPHS ####################

data_05 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_05[[3]]),
  group = "\u03C4 = 0.5"
)
save(data_05, file = "data05_naive.rda")

data_01 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_01[[3]]),
  group = "\u03C4 = 0.1"
)
save(data_01, file = "data01_naive.rda")

data_09 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_09[[3]]),
  group = "\u03C4 = 0.9"
)
save(data_09, file = "data09_naive.rda")

data_07 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_07[[3]]),
  group = "\u03C4 = 0.7"
)
save(data_07, file = "data07_naive.rda")

data_03 <- data.frame(
  name = counts_model$model,
  value = rowSums(sample_size_03[[3]]),
  group = "\u03C4 = 0.3"
)
save(data_03, file = "data03_naive.rda")

expression(alpha)
combined_data <- rbind(transform(data_01),
                       transform(data_03),
                       transform(data_05),
                       transform(data_07))

# Bar plot

library(extrafont)


ggplot(combined_data, aes(x = as.factor(name), y = value, fill = as.factor(name))) + 
  geom_bar(stat = "identity", width = 0.4) +
  facet_wrap(~group) +
  coord_flip() +
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
        text = element_text(family = "Times New Roman")) +
  guides(fill = guide_legend(ncol = 3, title = NULL, title.theme = element_text(size = 17)))  # Adjust the number of columns in the legend

ggsave("emcs_naive.pdf", width = 14, height = 8.5, device = cairo_pdf)



