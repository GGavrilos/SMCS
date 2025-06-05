############################# SMCS - Case Study 1 #############################
##################### Application to the Covid-Hub Dataset ####################

################----------- Data Preprocessing ---------------- ##############

library(dplyr)

load("forecasts_death.rda")
load("truth.rda") # Load forecasts and true values of deaths

# Keep only the relevant columns. #
forecasts_death <- forecasts_death[,c("model", "forecast_date", "target_end_date", "quantile", "value")]

# Remove rows with NAs (point forecasts = median).
forecasts_death <- na.omit(forecasts_death)

# Keep q = 0.05, 0.25, 0.5, 0.75, 0.95.
# forecasts_death <- forecasts_death[forecasts_death$quantile %in% c(0.05, 0.25, 0.5, 0.75, 0.95), ]

forecasts_death$model <- as.factor(forecasts_death$model) # Transform "model" to a factor. #
table(forecasts_death$model) # Get overview of models - Optional. #

# Remove models with very few predictions (<1800). #
forecasts_death <- forecasts_death[!(as.numeric(forecasts_death$model) %in% which(table(forecasts_death$model)<1800)),]

# Drop the corresponding levels from the factor "model" (the ones corresponding to the excluded models). #
forecasts_death$model <- droplevels(forecasts_death$model)

# Keep only the predictions that align with the timeline of the true data.
# This is because some models submitted predictions also for different dates from the ones on which true data was reported. #
forecasts_death <- forecasts_death[forecasts_death$target_end_date %in% truth$target_end_date, ]

# After removing some more predictions, some models have ver few submissions now.
# So, we remove models with few predictions (<1800) again.
forecasts_death <- forecasts_death[!(as.numeric(forecasts_death$model) %in% which(table(forecasts_death$model)<1800)),]

# Keep only the common dates between all models - Again, that's because there are several mismatches among the forecast dates. #
# reference_level <- counts_model[counts_model[,2]==min(counts_model[,2]),1]$model # model with the fewest values - Unused. #
# intersect_dates <- unique(forecasts_death[forecasts_death$model == reference_level,]$target_end_date) - Unused. #
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

# Filter out the rest of the dates. #
forecasts_target <- forecasts_death[(forecasts_death$target_end_date %in% intersect_end_dates) & (forecasts_death$forecast_date %in% intersect_forecast_dates), ]

# Delete duplicate rows (multiple predictions for the same date). #
forecasts_target <- forecasts_target[!(duplicated(forecasts_target[,c(1,3,4)])),]

# Again remove models with few predictions (<1800). #
forecasts_target <- forecasts_target[!(as.numeric(forecasts_target$model) %in% which(table(forecasts_target$model)<1800)),]
forecasts_target$model <- droplevels(forecasts_target$model)

# Remove models with non conforming dates. #
forecasts_target <- forecasts_target[forecasts_target$model %in% c("COVIDhub-ensemble", "PSI-DRAFT", "COVIDhub-baseline", "GT-DeepCOVID",
                                                                   "MOBS-GLEAM_COVID", "COVIDhub_CDC-ensemble"), ]

# forecasts_target <- forecasts_target[forecasts_target$model %in% c("COVIDhub-ensemble", "COVIDhub_CDC-ensemble", "COVIDhub-baseline", "GT-DeepCOVID", "MOBS-GLEAM_COVID", "CU-scenario_mid"), ]
# Update levels of factor "model".
forecasts_target$model <- droplevels(forecasts_target$model)

# Discard irrelevant dates from "truth". #
truth <- truth[truth$target_end_date %in% unique(forecasts_target$target_end_date), ]

# Discard irrelevant columns from "truth". #
truth <- truth[,c("model", "target_end_date", "value")]
save(truth, file = "truth.rda")

# Merge "truth" with "forecast_target". #
missing_cols <- setdiff(names(forecasts_target), names(truth))
truth[missing_cols] <- 0.5
forecasts_truth <- rbind(forecasts_target, truth)

# Rename the models. #
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

# Get overview of models and number of forecasts - Optional. #
counts_model <- forecasts_target %>% count(model) # Overview of counts of factor "model". #
