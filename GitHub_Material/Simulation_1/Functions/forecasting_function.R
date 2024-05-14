#################### Forecasting Function ####################
################### Used in Simulation 1 #####################
####### Performs E-value adjustment and forms the SMCS #######

forecasting_function <- function(alpha, N, epsilon, delta){
  # Prepare storage objects
  parameters <- expand.grid(epsilon=epsilon, delta=delta) # Grid of deviation parameters "delta" and "epsilon". #
  m = dim(parameters)[1] # No. of models. #
  E = matrix(1, nrow=m, ncol=m) # Array to store the e-processes for each pair. #
  d = matrix(0, nrow=m, ncol=m) # Array to store the score differences - We need them to tune lambda. #
  lambda = matrix(0, nrow=m, ncol=m) # Array to store the lambda values. #
  c = matrix(0, nrow=m, ncol=m) # Array to store the c parameters (the conditional bounds). #
  survivor <- c() # Track number of models left. #
  freq <- c() # Frequency of coverage of correct model - The method is conservative, so this is always equal to 1 in our simulation. #
  samsize <- c() # Time until termination - When there is one model left, the process terminates - We keep track of the termination time just for reference. #
  
  parameters_upd <- parameters # After each round, the deviation parameters of the excluded models will also be excluded. #
  
  y_old <- 0 # We start with a reference value y = 0. #
  for (n in 1:N){ # For each time point
    y_new = rnorm(1, mean = y_old, sd=1) # Draw a new outcome y. #
    if (m >= 2){ # If there are at least two models left. #
      # Define the updated deviation parameters. #
      eps_upd <- parameters_upd$epsilon
      delta_upd <- parameters_upd$delta
      L <- crps_norm(y_new, mean = y_old + eps_upd, sd = 1 + delta_upd) # Compute loss values. #
          
      d <- outer(L,L,'-') # Compute all the loss differences. #
      E <- E * (1 + lambda * d) # Update E-values. #
      
      # Compute the conditional bounds - These will be used in the next round. #
      for (i in 1:m){
        for (j in 1:m){
        c[i,j] <- subgau(mean1 = y_new + eps_upd[i], sd1 = 1 + delta_upd[i], mean2 = y_new + eps_upd[j], sd2 = 1 + delta_upd[j]) # Use the function loaded in the beginning. #
        }
      }
      
      lambda <- (2*c)^(-1) # Choose the tuning parameters based on the conditional bounds computed above. #
      
      y_old = y_new # Previously "new" observation now becomes "old". #
      
      E[is.nan(E)] <- 0 # Remove NAs from the E-value matrix. #
      lambda[lambda == "Inf"] <- 0 # Remove "Inf" from the array of lambda values. #
      diag(E) <- 0 # Set diagonal elements of the E-value matrix to zero - We are not interested on these elements. #
      
      # We now have all the E-values for this round and need to adjust them. #
      ###################### Adjustment Process begins. ######################
      ###### We follow the adjustment process proposed by Vovk & Wang. #######
      
      K = m # No. of E-values. #
      e = (m/(m-1))*rowMeans(E) # Rearrange E-values into a vector. #
      e_sorted = sort(e) # Sort the E-values. #
      order_e = order(e) # Keep track of the permutation that sorts the E-values as above. #
      S = rep(e_sorted[1],K) #  Vector to store the cumulative sums of the sorted E-values - these sums are used in the adjustment process (see Vovk & Wang). #
      e_adj = rep(NA,K) # Store the adjusted E-values here. #
      
      for (i in 2:K){
        S[i] = S[i-1]+e_sorted[i] # Cumulative sums of sorted E-values. #
      }
      
      for (i in 1:K){
        e_adj[order_e[i]]= min(((e_sorted[i]+S)/(1+(1:K)))[1:i]) # Vovk & Wang's algorithm. #
      }
      
      removed <- c() # Track the models that are excluded in this round.
      for (i in 1:m){
        if(e_adj[i] > 1/alpha){ # Rejection criterion - E-value exceeds 1/a. #
          removed <- c(removed, i)
        }
      }
      
      if (length(removed) >= 1){ # If at least one model is removed
        parameters_upd <- parameters_upd[-removed,] # Update the parameters
        E <- E[-removed,-removed] # Update the E-value matrix by excluding the rows/columns corresponding toexcluded models. #
        d <- d[-removed,-removed] # Update the loss difference matrix likewise. #
        c <- c[-removed,-removed] # Update the matrix of conditional bounds likewise. #
        lambda <- lambda[-removed,-removed] # -//- #
      }
      
      m = m - length(removed) # Redefine m - No. of remaining models. #
      survivor <- c(survivor, m) # Keep track of how many models have "survived". #
    }
    else{
      break
    }
    
    # Report MCS at the current time point. #
    if (min(abs(parameters[,1])+abs(parameters[,2])) == 0){ # If the correct model - i.e. epsilon = delta = 0 - has not been excluded, add 1 to the coverage frequency vector.
      freq <- c(freq,1)
    }
    else {
      freq <- c(freq,0)
    }
    samsize <- c(samsize,m) # Track the No. of remaining models.
  }
  samsize <- c(samsize, rep(samsize[length(samsize)], N-length(samsize))) # If only one model has remained, fill in the rest of the vector with the number of remaining models.
  freq <- c(freq, rep(freq[length(freq)], N-length(freq))) # Same for the coverage frequency vector.
  return(data.frame(samsize, freq))
}