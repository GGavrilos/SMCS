#################### Simulation 1 ####################
################# Strong Hypothesis ##################
###### Details in Example 2.3 of the pre-print #######

library(MASS)
library(scoringRules)
library(tidyr)
library(purrr)

## Load functions - REPLACE WITH LOCAL PATHS ##

## Function for the computation of the conditional bound c. ##
source("/Users/ggavrilopoul/polybox - Georgios Gavrilopoulos (georgios.gavrilopoulos@stat.math.ethz.ch)@polybox.ethz.ch/PhD/Projects/SMCS/RCode/Simulation/Functions/function_c.R")

## Function for the E-value adjustment and the formation of the SMCS. ##
source("/Users/ggavrilopoul/polybox - Georgios Gavrilopoulos (georgios.gavrilopoulos@stat.math.ethz.ch)@polybox.ethz.ch/PhD/Projects/SMCS/RCode/Simulation/Functions/forecasting_function.R")

##### Main Part #####

## Set parameters ##
alpha = 0.1 # Significance level. #
N = 1000 # No. of time steps. #
iter <- 100 # No. of simulation iterations. #

size <- matrix(0, nrow = iter, ncol = N) # Size of MCS over time. #
freq <- matrix(0, nrow = iter, ncol = N) # 1 if the optimal model belongs to the MCS - 0 else. #

epsilon = delta = seq(-0.6,0.6,by = 0.2) # Deviation parameters of the forecasters. #

for (k in 1:iter){
  set.seed(k)
  output <- forecasting_function(alpha, N, epsilon, delta) # Apply the forecasting function (loaded earlier) for all combinations of delta and epsilon. #
  size[k,] <- t(output[,1]) # Size of the MCS over time. #
  freq[k,] <- t(output[,2]) # Frequency of coverage of the truly optimal model - The method is conservative, so this is alway equal to 1 in our simulations. #
}


mcs_fwer <- c(t(colMeans(size))) # Average size of the MCS over time. #


# Export average size of MCS - REPLACE WITH LOCAL PATHS
save(mcs_fwer, file="/Users/ggavrilopoul/polybox - Georgios Gavrilopoulos (georgios.gavrilopoulos@stat.math.ethz.ch)@polybox.ethz.ch/PhD/EMCS/RCode/Results/mcs_fwer.rda")


