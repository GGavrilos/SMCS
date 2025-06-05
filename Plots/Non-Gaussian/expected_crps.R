################## ------------- Simulation 1 --------------- ##################
########### ---------- Time-Dependence of Expected CRPS ----------- ############

library(MASS)
library(scoringRules)
library(EnvStats)

n = 1000 # Sample Size
iter = 5000 # No. of Monte Carlo iterations
epsilon = delta = seq(-0.6, 0.6, length.out = 7) # bias and dispersion errors
parameters = expand.grid(epsilon = epsilon, delta = delta)


# ------------------------------------------------------------------------------
# Gaussian Distribution
# Choose an arbitrary model, e.g. the one with index "2".

index <- 1

index_new <- 49

y <- matrix(NA, nrow = iter, ncol = n)
crps <- matrix(NA, nrow = iter, ncol = n)

y[, 1] <- rnorm(iter, mean = 0, sd = 1)
crps[, 1] <- crps_norm(y[, 1], mean = parameters[index, 1], sd = 1 + parameters[index, 2])

for (j in 2:n){
  y[, j] <- rnorm(iter, mean = y[j - 1], sd = 1)
  crps[, j] <- crps_norm(y[, j], mean = y[j - 1] + parameters[index, 1], sd = 1 + parameters[index, 2])
}

exp_crps <- colMeans(crps)
plot(exp_crps, type = "l", col = "red", ylim = c(0.6, 0.8), ylab = "Expected CRPS (Gaussian)")

# ------------------------------------------------------------------------------
# Mixture of Gaussian Distributions
# Choose the same model as before, i.e. the one with index 2

y_mix <- matrix(NA, nrow = iter, ncol = n)
crps_mix <- matrix(NA, nrow = iter, ncol = n - 1)

p_mix <- 0.5 # Mixture Weight
p_matrix <- matrix(c(p_mix, 1 - p_mix), nrow = 1, byrow = TRUE)

mu<- c(0, 0)
sd <- c(1, 1)

y_mix[, 1] <- rnormMix(iter, mean1 = mu[1], mean2 = mu[2],
                       sd1 = sd[1], sd2 = sd[2],
                       p.mix = p_mix)

for (j in 2:n){
  
  mu <- cbind(atan(y_mix[, j - 1]), -atan(y_mix[, j - 1]))
  sd <- cbind(1 + sqrt(abs(y_mix[, j - 1])), 1 + sqrt(abs(y_mix[, j - 1])))
  
  for (i in 1:iter){
    
    y_mix[i, j] <- rnormMix(1, mean1 = mu[i, 1], mean2 = mu[i, 2],
                            sd1 = sd[i, 1], sd2 = sd[i, 2],
                            p.mix = p_mix)
    mu_i = matrix(mu[i, ] + parameters[index, 1], nrow = 1)
    sd_i = matrix(sd[i, ] + parameters[index, 2], nrow = 1)
    
    crps_mix[i, j - 1] <- crps_mixnorm(y_mix[i, j], m = mu_i,
                                       s = sd_i,
                                       w = p_matrix)
  }
}

exp_crps_mix <- colMeans(crps_mix)
plot(exp_crps_mix, type = "l", col = "blue", ylab = "Expected CRPS (Gaussian Mixture)", xlab = "Time")

# ------------------------------------------------------------------------------
# Mixture of Gaussian Distributions
# Choose the same model as before, i.e. the one with index 2

y_mix <- matrix(NA, nrow = iter, ncol = n)
crps_mix_new <- matrix(NA, nrow = iter, ncol = n - 1)

p_mix <- 0.5 # Mixture Weight
p_matrix <- matrix(c(p_mix, 1-p_mix), nrow = 1, byrow = TRUE)

mu<- c(0, 0)
sd <- c(1, 1)

y_mix[, 1] <- rnormMix(iter, mean1 = mu[1], mean2 = mu[2],
                       sd1 = sd[1], sd2 = sd[2],
                       p.mix = p_mix)

for (j in 2:n){
  
  mu <- cbind(atan(y_mix[, j - 1]), -atan(y_mix[, j - 1]))
  sd <- cbind(1 + sqrt(abs(y_mix[, j - 1])), 1 + sqrt(abs(y_mix[, j - 1])))
  
  for (i in 1:iter){
    
    y_mix[i, j] <- rnormMix(1, mean1 = mu[i, 1], mean2 = mu[i, 2],
                            sd1 = sd[i, 1], sd2 = sd[i, 2],
                            p.mix = p_mix)
    mu_i = matrix(mu[i, ] + parameters[index_new, 1], nrow = 1)
    sd_i = matrix(sd[i, ] + parameters[index_new, 2], nrow = 1)
    
    crps_mix_new[i, j - 1] <- crps_mixnorm(y_mix[i, j], m = mu_i,
                                       s = sd_i,
                                       w = p_matrix)
  }
}

exp_crps_mix_new <- colMeans(crps_mix_new)
plot(exp_crps_mix_new, type = "l", col = "red", ylab = "Expected CRPS (Gaussian Mixture)", xlab = "Time")
