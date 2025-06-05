################### IMPLEMENTATION OF TRADITIONAL MCS #######################
####################### NON-SEQUENTIAL SETTINGS #############################

## install.package("scoringRules")
library(EnvStats)
library(scoringRules)
library(MCS)

id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(id)

## Set parameters ##
Time = 1000 # No. of time steps.
p <- 0.5 # Mixture weight.

# Deviation parameters (mean - variance)
epsilon <- seq(-0.6, 0.6, by = 0.2) 
delta <- seq(-0.6, 0.6, by = 0.2)
parameters <- expand.grid(epsilon = epsilon, delta = delta) # Grid of deviation parameters". 
parameters$Model <- as.character(paste0("model_", 1:length(epsilon)**2))

crps_matrix <- matrix(0, nrow = Time, ncol = nrow(parameters))
colnames(crps_matrix) <- parameters$Model

y_old <- rnormMix(1, mean1 = 0, mean2 = 0,
                  sd1 = 1, sd2  = 1,
                  p.mix = p) # Observe first outcome. 
y <- c(y_old) # Store outcomes in vector y.

mcs_hansen <- rep(length(epsilon) * length(delta), Time)
coverage <- rep(1, Time)

parameters_upd <- parameters # Parameters of remaining. #
m <- length(parameters_upd$epsilon)

## Start with a burn in phase
for (t in 2:50){
  sqrt_abs_old <- sqrt(abs(y_old))
  
  mu_new <- c(atan(y_old), -atan(y_old))
  sd_new <- sqrt(c(1 + sqrt_abs_old, 1 + sqrt_abs_old))
  
  y_new <- rnormMix(1, mean1 = mu_new[1], mean2 = mu_new[2],
                    sd1 = sd_new[1], sd2  = sd_new[2],
                    p.mix = p) # Draw a new outcome y. #
  
  y[t] <- y_new # Append new outcome at the end of the outcome vector
  
  eps_upd <- parameters_upd$epsilon
  delta_upd <- parameters_upd$delta
  
  p_matrix <- matrix(rep(c(p, 1 - p), each = m), nrow = m) # parameter needs to have the right dimension
  
  L <- crps_mixnorm(rep(y_new, m), m = cbind(y_old + eps_upd, -y_old + eps_upd),
                    s = sqrt(cbind(1 + sqrt_abs_old + delta_upd, 1 + sqrt_abs_old + delta_upd)),
                    w = p_matrix) # Compute loss values. #
  
  crps_matrix[t, ] <- L
  
  y_old <- y_new # The new outcome now becomes old
}

for (t in 51:Time){
  m <- length(parameters_upd$epsilon) # No. of remaining models
  
  mu_new <- c(atan(y_old), -atan(y_old))
  sd_new <- sqrt(c(1 + sqrt(abs(y_old)), 1 + sqrt(abs(y_old))))
  y_new <- rnormMix(1, mean1 = mu_new[1], mean2 = mu_new[2],
                    sd1 = sd_new[1], sd2  = sd_new[2],
                    p.mix = p) # Draw a new outcome y. #
  
  y[t] <- y_new # Append new outcome at the end of the outcome vector
  
  p_matrix <- matrix(rep(c(p, 1 - p), each = m), nrow = m)
  
  L <- crps_mixnorm(rep(y_new, m), m = cbind(y_old + eps_upd, -y_old + eps_upd),
                    s = sqrt(cbind(1 + sqrt(abs(y_old)) + delta_upd, 1 + sqrt(abs(y_old)) + delta_upd)),
                    w = p_matrix) # Compute loss values. #
  
  crps_matrix[t, ] <- L
  y_old <- y_new # The new outcome now becomes old
  
  mcs_output <- MCSprocedure(Loss = crps_matrix[2:t, ], alpha = 0.1, B = 2000, statistic = "TR") # Run MCS algorithm
  
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

