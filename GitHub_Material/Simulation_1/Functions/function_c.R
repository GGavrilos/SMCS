################ Function that computes the predictable bounds c_t in the forecasting simulation #################
### As mentioned in Lemma B1. of the preprint, these are computed based on the crossing points of the two CDFs.###

library(scoringRules)


diff = function(mu1, sig1, mu2, sig2, x){
  d = pnorm(x, mean = mu1, sd = sig1) - pnorm(x, mean = mu2, sd = sig2) # Compute Difference of CDFs. #
  return(d)
}

subgau = function(mean1, sd1, mean2, sd2){
    r <- c((mean1 * sd2 - mean2 * sd1)/(sd2 - sd1 + 10^(-6)), -10^9, 10^9) # Compute crossing point and add +Inf and -Inf, as explained in Lemma B1. #
    c <- max(abs(crps_norm(r, mean = mean1, sd = sd1) - crps_norm(r, mean = mean2, sd = sd2))) # Compute maximum value attained on crossing points. #
  return(c)
}

