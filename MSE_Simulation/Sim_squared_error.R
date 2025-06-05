################################################################################
##########################  Simulation squared error ###########################
################################################################################

#-------------------------------------------------------------------------------
# load functions
source("Adjusting.R")
source("functions.R")

#-------------------------------------------------------------------------------
# set parameters

alpha = 0.1 # confidence level
n = 1000 # sample size
epsilon = delta = seq(-0.5,0.5,length.out=3) # bias and dispersion erros
parameters = expand.grid(epsilon=epsilon, delta=delta)
# model number i, corresponds to the i-th row in parameters
eps = parameters$epsilon
delta= parameters$delta

ind_sup_model = which((eps==0)&(delta==min(delta))) # indices of the superior model 

m = dim(parameters)[1] # number of models

#-------------------------------------------------------------------------------
# run simulation N_sim = 1000 times (specified in job1.sh)

id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(id)

#-------------------------------------------------------------------------------
# Generate data

X = matrix(NA, nrow=n,ncol=m)
for (i in 1:m){
  X[, i] = rnorm(n, mean = eps[i], sd = sqrt(1 + delta[i]))
}
# At all Sundays, make the prediction of ind_sup_model worse:
k = 7
tt = k*(1:floor(n/k))
err_sun = 0.6
X[tt,ind_sup_model] = rnorm(length(tt), mean = err_sun,
                            sd = sqrt(1 + delta[ind_sup_model])) 
L = X^2


###############################################################
#################### computation of SMCS ######################
###############################################################
#-------------------------------------------------------------------------------
# Pairwise loss-differences
d =array(0, c(m, m, n)) # loss differences
for (i in 1:m){
  for (j in (1:m)[-i]){
    d[i, j, ]= L[, i]- L[, j]
  }
}

max_diff =max(d)

#-------------------------------------------------------------------------------
# parameters for the sub exponential properties
c = lambda_e_process = matrix(NA, ncol = m, nrow = m) # loss differences
for (i in 1:m){
  for (j in (1:m)[-i]){
    c[i, j]= 4 * (1 + max(delta[c(i, j)]))
    lambda_e_process[i, j] = 1/(2 * c[i, j])
  }
}


#-------------------------------------------------------------------------------
# Pairwise e-processes
Delta_hat = V = E = E_sup=array(0, c(m, m, n)) 
#' arrays to store the accumulated losses and the e-processes

for (i in 1:m){
  for (j in (1:m)[-i]){
    Delta_hat[i, j, ]=cumsum(d[i, j, ])
    E[i, j, ]= pmin(1e+10, exp(lambda_e_process[i,j] * Delta_hat[i, j, ] - lambda_e_process[i, j]^2 * (1:n) * (2 * (1 + delta[i]) + 2 * (1 + delta[j]))^2/2))
    E_sup[i, j, ] = cummax(E[i, j, ]) # take the running supremum
    }
}


#-------------------------------------------------------------------------------
# p-abased approach by the geometric mean
p_adj_inv = array(0, c(m, m, n)) 
for (t in 1:n){
  p_adj_inv[,, t] = vector_to_matrix(adj_geom(matrix_to_vector(E_sup[, , t])))
}

#-------------------------------------------------------------------------------
# e-abased approach by the arithmetic mean
EE = E_adj = matrix(0, ncol = n, nrow = m) 
for (i in 1:m){
  EE[i, ]= colMeans(E[i, -i,])
}
E_adj=apply(EE, 2, adj)

#-------------------------------------------------------------------------------
# Compute model confidence sets
MCS_p = MCS_e = rep(list(integer(0)), n)
names(MCS_e) = names(MCS_p) = paste0("t=", 1:n)
MCS_e[[1]] = MCS_p[[1]] = 1:m

size_e = freq_e = size_p = freq_p = rep(NA, n)
size_e[1] = size_p[1] = m
freq_e[1] = freq_p[1] = 1

for (t in 2:n){
  MCS_p[[t]] = which(rowSums(p_adj_inv[, , t] < 1/alpha) == m)
  MCS_p[[t]] = MCS_p[[t]][is.element(MCS_p[[t]], MCS_p[[t-1]])]
  size_p[t] = length(MCS_p[[t]])
  freq_p[t] = is.element(ind_sup_model, MCS_p[[t]])
  MCS_e[[t]]= which(E_adj[,t] < 1/alpha)
  MCS_e[[t]] = MCS_e[[t]][is.element(MCS_e[[t]], MCS_e[[t-1]])]
  size_e[t] = length(MCS_e[[t]])
  freq_e[t] = is.element(ind_sup_model, MCS_e[[t]])
}



#-------------------------------------------------------------------------------
# Export-files
export = list("size_e" = size_e, 
              "freq_e" = freq_e,
              "E_adj" = E_adj,
              "size_p" = size_p, 
              "freq_p" = freq_p,
              "p_adj_inv" = p_adj_inv) 

save(export, file = paste0("Simulation=", id,".rda"))
