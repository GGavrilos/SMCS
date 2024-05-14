############################# SMCS - Case Study 1 #############################
##################### Application to the Covid-Hub Dataset ####################
################## Function that tests the strong hypothesis ##################


covid_quantile <- function(forecast_data, truth_data, quantile_value, alpha){
  # install.packages("matricks")
  # install.packages("greybox")
  library(matricks) # Contains function to split the dataset so that we can get the pair of models of interest. #
  library(greybox) # This package contains a built-in quantile score function. #
  m <- length(levels(forecast_data$model)) # No. of models. #
  
  E <- matrix(1, nrow=m, ncol=m) # Array to store the E-processes for each pair of models. #
  d <- matrix(0, nrow=m, ncol=m) # Array to store the loss differences. #
  
  samsize <- c() # Sample size until termination. #
  multi <- (2 - abs(quantile_value - 1/2))/(1 + abs(quantile_value - 1/2)) # Multiplicative factor - See Subsection 5.1.2 of the preprint for details. #
  
  Time <- nrow(truth_data) # number of time points
  data_target <- forecast_data[forecast_data$quantile == quantile_value,]
  
  # Split the big data frame into smaller ones, one for each model. #
  forecasts_splits <- split(data_target, data_target$model)
  
  forecast_data$model <- as.factor(forecast_data$model) # Transform "model" to a factor. #
  model_in <- matrix(0, nrow = m, ncol = Time) # Matrix to store whether each model is in the MCS. #
  
  for (t in 2:Time){
    if (m>1){
      unsplited <- bind_rows(forecasts_splits) # Re-bind the split versions from above. #
      unsplited$model <- droplevels(unsplited$model) # Throw away the excluded models. #
      unsplited$model <- as.factor(unsplited$model) # Transform "model" to factor. #
      
      model_in[, t] <- (levels(forecast_data$model) %in% levels(unsplited$model)) # 1 = model is in the MCS at time t / 0 = otherwise. #
      for (i in 1:m){
        for (j in 1:m){
          L_it <- pinball(holdout = log(truth_data[t,]$value),
                          forecast = log(10^(-6) + as.numeric(forecasts_splits[[i]][t,"value"])),
                          level = quantile_value) ## Loss of first model. #
          L_jt <- pinball(holdout = log(truth_data[t,]$value),
                          forecast = log(10^(-6) + as.numeric(forecasts_splits[[j]][t,"value"])),
                          level = quantile_value) ## Loss of first model. #
          # Compute predictable bound. #
          c <- max(quantile_value, 1 - quantile_value) * abs(log(10^(-6) + as.numeric(forecasts_splits[[i]][t,"value"])) - log(10^(-6) + as.numeric(forecasts_splits[[j]][t,"value"])))
          
          ## Compute predictable multiplying factor.
          K <- ((2 - abs(quantile_value - 1/2))/(1 + abs(quantile_value - 1/2))) * (1/pi) * (3*pi/2 + atan(-d[i,j]))
          lambda <- 1/(K * c + 10^(-6)) # predictable adjusting of hyperparameter - REPLACE WITH NEXT LINE IF YOU WANT THE NAIVE SETTING. #
          # lambda = 1/(2 * c + 10^(-6)) # naive choice for hyperparameter - REPLACE WITH PREVIOUS LINE IF YOU WANT THE PREDICTABLE SETTING. #
          d[i,j] <- L_it - L_jt
          E[i,j] <- E[i,j] * (1 + lambda * d[i,j])
          }
      }
      E[is.nan(E)] <- 0 # Set NANs to zero in the E-value matrix. #
      diag(E) <- 0 # Set diagonal elements of E-value matrix to zero, because we will have to take averages later. #
      
      # We now have all the E-values for this round and need to adjust them. #
      ####### We use the adjustment process proposed by Vovk & Wang. #########
      
      K = m # No. of E-values. #
      e = (m/(m-1))*rowMeans(E) # Rearrange E-values into a vector. #
      e_sorted = sort(e) # Sort the E-values. #
      order_e = order(e) # Keep track of permutation that sorts the E-values. #
      S = rep(e_sorted[1],K) # Array to store the cumulative sums of sorted E-values - We need them in the adjustment process. #
      e_adj = rep(NA,K) # Array to store the adjusted E-values later. #
      
      for (i in 2:K){
        S[i] = S[i-1]+e_sorted[i] # Partial sums of sorted e-values. #
      }
      
      for (i in 1:K){
        e_adj[order_e[i]]= min(((e_sorted[i]+S)/(1+(1:K)))[1:(i-1)]) # Vovk & Wang's algorithm. #
      }
      
      removed <- c() # Keep track of removed models.
      for (i in 1:m){
        if(e_adj[i] > 1/alpha){ # Rejection criterion - E-value exceeds 1/alpha. #
          removed <- c(removed, i)
        }
      }
      if (length(removed) >= 1){## If at least one model is removed
        E <- E[-removed,-removed] # Update the E-value matrix by excluding the rows/columns corresponding to the excluded models. #
        forecasts_splits <- forecasts_splits[-removed] # -//-
        m <- m - length(removed) # Update the number of models. #
      }
      }
      samsize <- c(samsize,m) # Keep track of the size of the MCS. #
  }
  return(list(samsize,forecasts_splits, model_in))
}