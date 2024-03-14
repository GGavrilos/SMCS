covid_quantile <- function(forecast_data, truth_data, quantile_value, alpha){
  # install.packages("matricks")
  # install.packages("greybox")
  library(matricks) ## contains function to split the dataset so that we can get the pair of models of interest
  library(greybox) ## this package contains a built-in quantile score function
  m <- length(levels(forecast_data$model)) # number of models
  
  E <- matrix(1, nrow=m, ncol=m) # store the e-processes for each pair
  d <- matrix(0, nrow=m, ncol=m) # store the loss differences
  
  samsize <- c() # sample size until termination
  
  multi <- (2 - abs(quantile_value - 1/2))/(1 + abs(quantile_value - 1/2)) ## multiplicative factor to be used later
  
  Time <- nrow(truth_data) # number of time points
  
  data_target <- forecast_data[forecast_data$quantile == quantile_value,]
  
  # split the big data frame into smaller ones, one for each model
  forecasts_splits <- split(data_target, data_target$model)
  
  forecast_data$model <- as.factor(forecast_data$model) # transform "model" to a factor
  model_in <- matrix(0, nrow = m, ncol = Time) # matrix to store whether each model is in the MCS
  
  for (t in 2:Time){
    if (m>1){
      unsplited <- bind_rows(forecasts_splits) # just re-binds the split version
      unsplited$model <- droplevels(unsplited$model) # erase 
      unsplited$model <- as.factor(unsplited$model) # transforms "model" to factor
      
      model_in[, t] <- (levels(forecast_data$model) %in% levels(unsplited$model)) # 1 = model is in the MCS at time t / 0 = otherwise
      for (i in 1:m){
        for (j in 1:m){
          L_it <- pinball(holdout = log(truth_data[t,]$value),
                          forecast = log(10^(-6) + as.numeric(forecasts_splits[[i]][t,"value"])),
                          level = quantile_value) ## loss of first model
          L_jt <- pinball(holdout = log(truth_data[t,]$value),
                          forecast = log(10^(-6) + as.numeric(forecasts_splits[[j]][t,"value"])),
                          level = quantile_value) ## loss of first model
          
          c <- max(quantile_value, 1 - quantile_value) * abs(log(10^(-6) + as.numeric(forecasts_splits[[i]][t,"value"])) - log(10^(-6) + as.numeric(forecasts_splits[[j]][t,"value"]))) ## predictable bound
          K <- ((2 - abs(quantile_value - 1/2))/(1 + abs(quantile_value - 1/2))) * (1/pi) * (3*pi/2 + atan(-d[i,j])) ## predictable adjusting of multiplying factor
          # lambda <- 1/(K * c + 10^(-6)) ## predictable adjusting of hyperparameter 
          lambda = 1/(2 * c + 10^(-6)) ## naive choice for hyperparameter
          d[i,j] <- L_it - L_jt
          E[i,j] <- E[i,j] * (1 + lambda * d[i,j])
          }
      }
      E[is.nan(E)] <- 0
      diag(E) <- 0 # because we will have to take averages
      
      ## We now have all the e-values for this round and need to adjust them
      K = m ## number of e-values
      
      e = (m/(m-1))*rowMeans(E) ## rearrange e-values into a vector
      e_sorted = sort(e) ## sort the e-values
      order_e = order(e) ## sorting permutation
      S = rep(e_sorted[1],K) ## start computing adjusted e-values as in Vovk & Wang
      e_adj = rep(NA,K) ## store the adjusted ones here
      
      for (i in 2:K){
        S[i] = S[i-1]+e_sorted[i] ## partial sums of sorted e-values
      }
      
      for (i in 1:K){
        e_adj[order_e[i]]= min(((e_sorted[i]+S)/(1+(1:K)))[1:(i-1)]) ## Vovk & Wang's algorithm
      }
      
      removed <- c()
      for (i in 1:m){
        if(e_adj[i] > 1/alpha){
          removed <- c(removed, i)
        }
      }
      if (length(removed) >= 1){## if at least one model is removed
        E <- E[-removed,-removed]## keep the values corresponding to the remaining models
        forecasts_splits <- forecasts_splits[-removed] ## -//-
        m <- m - length(removed) ## update the number of models
      }
      }
      samsize <- c(samsize,m)
  }
  return(list(samsize,forecasts_splits, model_in))
}