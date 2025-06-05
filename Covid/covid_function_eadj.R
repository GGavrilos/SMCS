############################# SMCS - Case Study 1 #############################
##################### Application to the Covid-Hub Dataset ####################
################## -------- E-Process Adjustment ----------- ##################

source("adjustment_functions.R")

# install.packages("matricks")
# install.packages("greybox")
library(matricks) # Function to split the dataset.
library(greybox) # Built-in quantile score function.

covid_quantile_eadj <- function(forecast_data, truth_data, quantile_value, alpha){
  # ----------------------------------------------------------------------------
  # Prepara storage objects
  m <- length(levels(forecast_data$model)) # No. of models. #
  
  E <- matrix(1, nrow = m, ncol = m) # Store E-processes for each pair of models.
  d <- matrix(0, nrow = m, ncol = m) # Store loss differences. #
  
  Time <- nrow(truth_data) # number of time points
  data_target <- forecast_data[forecast_data$quantile == quantile_value, ]
  
  # Split the big data frame into smaller ones, one for each model. #
  forecasts_splits <- split(data_target, data_target$model)
  forecast_data$model <- as.factor(forecast_data$model) # Transform "model" to a factor.
  
  model_in <- matrix(1, nrow = m, ncol = Time) # Store whether model is in the MCS.
  
  for (t in 2:Time){
    # --------------------------------------------------------------------------
    # Calculation of pairwise E-values
    
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
        lambda <- 1/(K * c + 10^(-6)) # predictable adjusting of hyperparameter
        # lambda = 1/(2 * c + 10^(-6)) # naive choice for hyperparameter
        d[i, j] <- L_it - L_jt
        E[i, j] <- E[i, j] * (1 + lambda * d[i, j])
      }
    }
    E[is.nan(E)] <- 0 # Set NANs to zero.
    diag(E) <- 0 # Set diagonal elements to zero.
      
    # --------------------------------------------------------------------------
    # E-adjustment process proposed by (Vovk & Wang)
    
    e = (m/(m - 1)) * rowMeans(E) # Rearrange E-values into a vector.
    e_adj = adj(e) # E-Value adjustment step (Vovk & Wang)
      
    for (i in 1:m){
      if(e_adj[i] > 1/alpha){ # Rejection criterion - E-value exceeds 1/alpha. #
        model_in[i, t] <- 0
      } else if (model_in[i, t - 1] < 0.1){
        model_in[i, t] <- 0
      }
    }
  }
  return(model_in)
}