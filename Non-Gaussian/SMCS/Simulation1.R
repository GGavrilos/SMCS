################################################################################
########################## Forecasting Simulation  #############################
################################################################################

#-------------------------------------------------------------------------------
# load packages and functions
# install.packages("EnvStats", "scoringRules")

library(MASS)
library(EnvStats)
library(scoringRules)

source("Adjusting.R")
source("function_c.R")

#-------------------------------------------------------------------------------
# Set parameters

alpha = 0.1 # confidence level
n = 1000 # sample size

p = 0.5 # mixing weight

p1 <- matrix(c(p, 1 - p), nrow = 1)
p2 <- matrix(c(p, 1 - p), nrow = 1)

epsilon = delta = seq(-0.6, 0.6, length.out = 7) # bias and dispersion errors
parameters = expand.grid(epsilon = epsilon, delta = delta)
# model number i, corresponds to the i-th row in parameters
eps = parameters$epsilon
delta= parameters$delta

ind_sup_model = which((eps == 0) & (delta == 0)) # indices of the superior model 

#-------------------------------------------------------------------------------
# Run multiple simulations

id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(id)

#-------------------------------------------------------------------------------
# Simulate data

y = rep(NA, n)
y[1] <- rnormMix(1, mean1 = 0, mean2 = 0,
                 sd1 = 1, sd2 = 1, p.mix = p) # We start with a reference value y. #

for (i in 2:n){
  mu_i <- c(atan(y[i - 1]), -atan(y[i - 1]))
  sd_i <- c(1 + sqrt(abs(y[i - 1])), 1 + sqrt(abs(y[i - 1])))
  
  y[i] <- rnormMix(1, mean1 = mu_i[1], mean2 = mu_i[2],
                    sd1 = sqrt(sd_i[1]), sd2  = sqrt(sd_i[2]),
                    p.mix = p) # Draw a new outcome y. #
}

#-------------------------------------------------------------------------------
# Prepare objects to store

m = dim(parameters)[1] # number of models 
L = matrix(NA, nrow = n, ncol = m) # to store the losses

means = c(0, atan(y[1 : (n - 1)])) # true means
variances = 1 + sqrt(abs(c(0, y[1 : (n - 1)]))) # true variances

p_matrix <- matrix(rep(c(p, 1 - p), each = n), nrow = n) # parameter needs to have the right dimension

#-------------------------------------------------------------------------------
# Losses of models

for (i in 1:m){
  L[, i] <- crps_mixnorm(y, m = cbind(means + parameters[i, 1], -means + parameters[i, 1]),
                    s = sqrt(cbind(variances + parameters[i, 2], variances + parameters[i, 2])),
                    w = p_matrix) # Compute loss values. #
}

#-------------------------------------------------------------------------------
# Calculate c-values

c = array(Inf, c(m, m, n))  # array to store the c parameters

for (t in 2:n){
  for (i in 1:m){
    for (j in 1:i){
      mu1 <- c(means[t - 1] + parameters[i, 1], -means[t - 1] + parameters[i, 1])
      mu2 <- c(means[t - 1] + parameters[j, 1], -means[t - 1] + parameters[j, 1])
      
      sigma1 <- c(variances[t - 1] + parameters[i, 2], variances[t - 1] + parameters[i, 2])
      sigma2 <- c(variances[t - 1] + parameters[j, 2], variances[t - 1] + parameters[j, 2])
      
      c[i, j, t] = c[j, i, t] = predictable_bound(mu1 = mu1, mu2 = mu2,
                                                  sigma1 = sqrt(sigma1),
                                                  sigma2 = sqrt(sigma2),
                                                  p1 = p1, p2 = p2)
    }
  }
}

lambda = 1/(2 * c)
#-------------------------------------------------------------------------------
# Pairwise loss-differences

d = array(0, c(m, m, n))

for (i in 1:m){
  for (j in (1:m)[-i]){
    d[i, j, ] = L[, i] - L[, j]
  }
}

#-------------------------------------------------------------------------------
# Pairwise e-processes

E = E_sup = array(1, c(m, m, n))
# arrays to store the accumulated losses, the variance processes and the e-processes

for (i in 1:m){
  for (j in (1:m)[-i]){
   E[i, j, ] = cumprod(1 + lambda[i, j, ] * d[i, j, ])
   E_sup[i, j, ] = cummax(E[i, j, ])
  }
}

#-------------------------------------------------------------------------------
# e-based approach by the arithmetic mean

EE = E_adj = matrix(1, ncol = n, nrow = m)

for (i in 1:m){
  EE[i,] = colMeans(E[i, -i,])
}

E_adj = apply(EE, 2, adj)

#-------------------------------------------------------------------------------
# Compute model confidence sets

MCS_e = rep(list(integer(0)), n)
names(MCS_e) = paste0("t=", 1:n)
MCS_e[[1]] = 1:m
size_e = freq_e = rep(NA, n)
size_e[1] = m
freq_e[1] = 1

for (t in 2:n){
  if (size_e[t - 1] > 1){
    MCS_e[[t]]= which(E_adj[, t] < 1/alpha)
    MCS_e[[t]] = MCS_e[[t]][is.element(MCS_e[[t]], MCS_e[[t - 1]])]
    size_e[t] = length(MCS_e[[t]])
    freq_e[t] = is.element(ind_sup_model, MCS_e[[t]])
  } else{
    MCS_e[[t]] = MCS_e[[t - 1]]
    size_e[t] = size_e[t - 1]
    freq_e[t] = freq_e[t - 1]
  }
}

#-------------------------------------------------------------------------------
# Export-files

export = list("size_e" = size_e, 
              "freq_e"=freq_e,
              "E_adj"=E_adj) 

save(export, file=paste0("Simulation=", id, ".rda"))

