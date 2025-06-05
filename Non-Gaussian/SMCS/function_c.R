############# Predictable bounds c_t in the forecasting simulation #############
###--------- Computed based on the crossing points of the two CDFs...--------###

library(stats)
library(scoringRules)

#-------------------------------------------------------------------------------
# Maximize function over grid.

predictable_bound <- function(mu1, mu2, sigma1, sigma2, p1, p2){
    extreme <- 1e10
    y_sequence <- c(-extreme,
                    seq(min(mu1, mu2) - 20 * max(sigma1, sigma2),
                        max(mu1, mu2) + 20 * max(sigma1, sigma2),
                        length.out = 400),
                    extreme)
    n = length(y_sequence)
    
    mu1 <- matrix(rep(mu1, each = n), ncol = 2)
    mu2 <- matrix(rep(mu2, each = n), ncol = 2)
    
    sigma1 <- matrix(rep(sigma1, each = n), ncol = 2)
    sigma2 <- matrix(rep(sigma2, each = n), ncol = 2)
    
    p1 <- matrix(rep(p1, each = n), ncol = 2)
    p2 <- matrix(rep(p2, each = n), ncol = 2)
    
    bound <- max(abs(crps_mixnorm(y_sequence, m = mu1, s = sigma1, w = p1) -
                       crps_mixnorm(y_sequence, m = mu2, s = sigma2, w = p2)))
    return(bound)
}

# ## Toy example
# 
# mu1 <- c(-3, 2)
# mu2 <- c(1, 5)
# 
# p1 <- c(0.5, 0.5)
# p2 <- c(0.5, 0.5)
# 
# sigma1 <- c(0.5, 1)
# sigma2 <- c(1, 2)
# 
# extreme <- 10
# x_sequence <- c(-extreme, seq(-10, 10, length.out = 1000), extreme)
# n = length(x_sequence)
# 
# mu1_matrix = matrix(rep(mu1, each = n), ncol = 2)
# mu2_matrix = matrix(rep(mu2, each = n), ncol = 2)
# 
# sigma1_matrix = matrix(rep(sigma1, each = n), ncol = 2)
# sigma2_matrix = matrix(rep(sigma2, each = n), ncol = 2)
# 
# p1_matrix = matrix(rep(p1, each = n), ncol = 2)
# p2_matrix = matrix(rep(p2, each = n), ncol = 2)
# 
# plot(x_sequence, abs(crps_mixnorm(x_sequence, m = mu1_matrix, s = sigma1_matrix,
#                               w = p1_matrix) - crps_mixnorm(x_sequence, m = mu2_matrix, s = sigma2_matrix,
#                               w = p2_matrix)), type = "l", xlab = "Y", ylab = "CRPS")
# 
# bound <- max(abs(crps_mixnorm(x_sequence, m = mu1_matrix, s = sigma1_matrix, w = p1_matrix) -
#                    crps_mixnorm(x_sequence, m = mu2_matrix, s = sigma2_matrix, w = p2_matrix)))
