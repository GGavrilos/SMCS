################### IMPLEMENTATION OF TRADITIONAL MCS #######################
####################### NON-SEQUENTIAL SETTINGS #############################

## install.package("scoringRules")
library(scoringRules)
library(MCS)

id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(id)

## Set parameters ##
Time = 1000 # No. of time steps.

# Deviation parameters (mean - variance)
epsilon <- seq(-0.6, 0.6, by = 0.2) 
delta <- seq(-0.6, 0.6, by = 0.2)
parameters <- expand.grid(epsilon = epsilon, delta = delta) # Grid of deviation parameters". 
parameters$Model <- as.character(paste0("model_", 1:length(epsilon)**2))

crps_matrix <- matrix(0, nrow = Time, ncol = nrow(parameters))
colnames(crps_matrix) <- parameters$Model

y <- rep(NA, Time)

y[1] <- 0

mcs_hansen <- rep(length(epsilon) * length(delta), Time)
coverage <- rep(1, Time)

parameters_upd <- parameters # Parameters of remaining. #
m <- length(parameters_upd$epsilon)

## Start with a burn in phase
for (t in 2:50){
  
  y[t] <- rnorm(1, mean = y[t - 1], sd = 1) # Draw a new outcome y. #
  
  crps_matrix[t, ] <- crps_norm(y[t], mean = y[t - 1] + parameters_upd[, 1],
                                sd = sqrt(1 + parameters_upd[, 2])) # Compute loss values. #
}

for (t in 51:Time){
  m <- length(parameters_upd$epsilon) # No. of remaining models

  y[t] <- rnorm(1, mean = y[t - 1], sd = 1) # Draw a new outcome y. # # Draw a new outcome y. #
  
  crps_matrix[t, ] <- crps_norm(y[t], mean = y[t - 1] + parameters_upd[, 1],
                                sd = sqrt(1 + parameters_upd[, 2])) # Compute loss values. #
  
  mcs_output <- MCSprocedure(Loss = crps_matrix[2:t, ], alpha = 0.1,
                             B = 2000, statistic = "TR") # Run MCS algorithm
  
  surviving_models <- rownames(mcs_output@show)  # Extract surviving models
  
  # Ensure the parameters data frame keeps only surviving models
  parameters_upd <- subset(parameters, Model %in% surviving_models)  
  
  # Correctly subset crps_matrix without dimension mismatch
  crps_matrix <- crps_matrix[, colnames(crps_matrix) %in% surviving_models,
                             drop = FALSE]
  
  # Ensure column names are correctly set before subsetting
  colnames(crps_matrix) <- parameters_upd$Model
  
  mcs_hansen[t] <- length(surviving_models)
  if (min(abs(parameters_upd[, 1]) + abs(parameters_upd[, 2])) < 0.1){
    coverage[t] <- 1
  }
  else{
    coverage[t] <- 0
  }
  if (length(surviving_models) == 1){ # Stop if only one model is remaining
    break
  }
}

mcs_hansen[t:Time] <- rep(mcs_hansen[t], Time - t + 1)
coverage[t:Time] <- rep(coverage[t], Time - t + 1)

export <- list(
  Frequency = coverage,
  Size_MCS = mcs_hansen
)

save(export, file = paste0("MCS=", id, ".rda"))

