############################# SMCS - Case Study 1 #############################
##################### Application to the Covid-Hub Dataset ####################
################## -------- P-Process Adjustment ----------- ##################

source("adjustment_functions.R")
source("functions.R")

# install.packages("matricks")
# install.packages("greybox")
library(matricks) # Function to split the dataset.
library(greybox) # Built-in quantile score function.

covid_quantile_padj <- function(forecast_data, truth_data, quantile_value, alpha){
  # ----------------------------------------------------------------------------
  # Prepare storage objects
  
  m <- length(levels(forecast_data$model)) # No. of models. #
  
  E <- matrix(1, nrow = m, ncol = m) # Pairwise e-processes.
  E_sup <- matrix(1, nrow = m, ncol = m) # Running supremum of e-processes.
  d <- matrix(0, nrow = m, ncol = m) # Loss differences.
  
  Time <- nrow(truth_data) # number of time points
  data_target <- forecast_data[forecast_data$quantile == quantile_value, ]
  
  # Split the big data frame into smaller ones, one for each model. #
  forecasts_splits <- split(data_target, data_target$model)
  forecast_data$model <- as.factor(forecast_data$model) # Transform "model" to a factor.
  
  model_in <- matrix(1, nrow = m, ncol = Time) # Store whether model is in the MCS.
  
  # ----------------------------------------------------------------------------
  # Calculation of pairwise E-values
  for (t in 2:Time){
    
    for (i in 1:m){
      for (j in 1:m){
        L_it <- pinball(holdout = log(truth_data[t,]$value),
                        forecast = log(10^(-6) + as.numeric(forecasts_splits[[i]][t, "value"])),
                        level = quantile_value) ## Loss of first model.
        L_jt <- pinball(holdout = log(truth_data[t,]$value),
                        forecast = log(10^(-6) + as.numeric(forecasts_splits[[j]][t, "value"])),
                        level = quantile_value) ## Loss of second model.
        # Compute predictable bound. #
        c <- max(quantile_value, 1 - quantile_value) * abs(log(10^(-6) + as.numeric(forecasts_splits[[i]][t,"value"])) - log(10^(-6) + as.numeric(forecasts_splits[[j]][t,"value"])))
        
        ## Compute predictable multiplying factor.
        K <- ((2 - abs(quantile_value - 1/2))/(1 + abs(quantile_value - 1/2))) * (1/pi) * (3 * pi/2 + atan(-d[i, j]))
        # lambda <- 1/(K * c + 10^(-6)) # predictable adjusting of hyperparameter
        lambda = 1/(2 * c + 10^(-6)) # naive choice for hyperparameter
        d[i, j] <- L_it - L_jt
        
        E_sup[i, j] <- max(E_sup[i, j], E[i, j] * (1 + lambda * d[i, j])) # Update pairwise sup only if necessary
        E[i, j] <- E[i, j] * (1 + lambda * d[i, j]) # Multiplicatively update E[i,j]
      }
    }
    
    # --------------------------------------------------------------------------
    # P-adjustment process
    
    p_adj_inv = vector_to_matrix(adj_geom(matrix_to_vector(E_sup)))
    model_in[, t] = (rowSums(p_adj_inv < 1/alpha) == m) # Vector of length m (1: model included, 0: model excluded)
  }
  
  return(model_in)
}