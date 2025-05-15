############################# SMCS - Case Study 1 #############################
##################### Application to the Covid-Hub Dataset ####################

############-------------------- DATA LOADING ---------------------############
############--------------------- Preparation -------------------- ############

#install dependencies and packages from the github page https://github.com/reichlab/covid19-forecast-hub
#install.packages("remotes")
#install.packages("devtools")
#remotes::install_github('reichlab/zoltr')
#remotes::install_github('epiforecasts/scoringutils')
#devtools::install_github('reichlab/covidHubUtils')
#remotes::install_github("Chicago/RSocrata")
#install.packages(c('tidyverse', 'rvest', 'here', 'covidcast'))
#remotes::install_github("reichlab/covidData")

library(remotes)
library(devtools)
library(dplyr)

library(covidData)
library(covidHubUtils)

# Download forecasts for the US. #
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

# Download true statistics from John Hopkins University (cases/deaths/hospitalizations). #
truth <- load_truth(
  truth_source = "JHU",
  hub = "US",
  locations = "US",
  target_variable = c("inc death"),
  temporal_resolution = "weekly"
)

save(forecasts_death, file = "forecasts_death.rda")
save(truth, file = "truth.rda")