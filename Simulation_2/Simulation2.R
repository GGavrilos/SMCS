################################################################################
################################## Simulation 2  ###############################
################################################################################

#-------------------------------------------------------------------------------
# load packages and functions

library(MASS)
library(scoringRules)

source("Adjusting.R")

#-------------------------------------------------------------------------------
# Set parameters

alpha = 0.1 # confidence level
n = 1000 # sample size
epsilon = delta = seq(-0.6, 0.6, length.out = 7) # bias and dispersion erros
parameters = expand.grid(epsilon=epsilon, delta=delta)
# model number i, corresponds to the i-th row in parameters
eps = parameters$epsilon
delta= parameters$delta

ind_sup_model = which((eps == 0) & (delta == 0)) # indices of the superior model 

#-------------------------------------------------------------------------------
# run simulation multiple times

id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(id)

#-------------------------------------------------------------------------------
# Simulate data

y = rep(NA,n)
y[1] = rnorm(1)
for (i in 2:n){
  y[i]=rnorm(1,mean=y[i-1])
}

#-------------------------------------------------------------------------------
# Prepare objects to store

m = dim(parameters)[1] # number of models 
L = matrix(NA,nrow=n,ncol=m) # to store the losses
means= c(0,y[1:(n - 1)]) # true means

#-------------------------------------------------------------------------------
# Losses of models

for (i in 1:m){
L[,i]= crps_norm(y, mean = means + eps[i], sd = sqrt(1 + delta[i]))
}
# At all Sundays, make the prediction of model 5 worse:

k = 7
tt = k*(1:floor(n/k))
rho = 0.3
L[tt, ind_sup_model] = crps_norm(y[tt], mean = means[tt] + rho, sd = sqrt(1 + rho))

#-------------------------------------------------------------------------------
# Calculate c-values

c1= matrix(NA, ncol = m, nrow = m) # array to store the c parameters on weekdays

high = 1e10
 for (i in 1:m){
  for (j in (1:m)[-i]){
 tmp = integer(0)
   if (delta[i] != delta[j]){
     tmp = ((eps[i]) * sqrt(1 + delta[j]) - (eps[j]) * sqrt(1 + delta[i]))/(sqrt(1 + delta[j])-sqrt(1+delta[i]))
   }
  c1[i,j]= max(abs(
    crps_norm(c(high, -high,tmp), mean = eps[i], sd = sqrt(1 + delta[i])) - crps_norm(c(high, -high, tmp), mean = eps[j], sd = sqrt(1 + delta[j]))))
     }
 }

c2 = c1

for (j in (1:m)[-ind_sup_model]){
  tmp = ((0.3) * sqrt(1 + delta[j])- (eps[j]) * sqrt(1.3))/(sqrt(1 + delta[j]) - sqrt(1.3))
  c2[ind_sup_model, j] = c2[j, ind_sup_model] = max(abs(crps_norm(c(high, -high, tmp), mean = 0.3, sd = sqrt(1.3)) - crps_norm(c(high, -high,tmp), mean = eps[j], sd = sqrt(1 + delta[j]))))
}

#-------------------------------------------------------------------------------
# Pairwise loss-differences

d = array(0, c(m,m,n)) 
for (i in 1:m){
  for (j in (1:m)[-i]){
    d[i, j, ]= L[, i]- L[, j]
  }
}

#-------------------------------------------------------------------------------
# Transformation to make loss-differences uniformly bounded

for (i in (1:n)[-tt]){
  d[, , i] = d[, , i]/c1
}
for (i in tt){
  d[, , i]=d[, , i]/c2
}

#-------------------------------------------------------------------------------
# Set uniform parameters

C = 2
lambda= 1/(2 * C)
psi_E_c = (-log(1 - C * lambda)-C * lambda)/C^2

#-------------------------------------------------------------------------------
# Pairwise e-processes

Delta_hat = V = E = E_sup = array(0, c(m, m, n)) 
# arrays to store the accumulated losses, the variance processes and the e-processes

for (i in 1:m){
  for (j in (1:m)[-i]){
    V[i,j,] = cumsum((d[i,j,] - cumsum(c(0, d[i, j, -n]))/c(1,1:(n - 1)))^2)
    Delta_hat[i,j,] = cumsum(d[i,j,])
    E[i,j,]= exp(lambda * Delta_hat[i,j,] - psi_E_c * V[i,j,])
    E_sup[i,j,] = cummax(E[i,j,]) # consider the running max
  }
}

#-------------------------------------------------------------------------------
# e-based approach by the arithmetic mean

EE = E_adj = matrix(0, ncol = n, nrow = m)
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
  MCS_e[[t]] = which(E_adj[,t] < 1/alpha)
  MCS_e[[t]] = MCS_e[[t]][is.element(MCS_e[[t]],MCS_e[[t-1]])]
  size_e[t] = length(MCS_e[[t]])
  freq_e[t] = is.element(ind_sup_model, MCS_e[[t]])
}

#-------------------------------------------------------------------------------
# Export-files

export = list("size_e" = size_e, 
              "freq_e"=freq_e,
              "E_adj"=E_adj)

save(export, file=paste0("Simulation=", id, ".rda"))
