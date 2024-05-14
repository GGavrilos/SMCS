################################################################################
############################### Simulation 3  ##################################
################################################################################


#-------------------------------------------------------------------------------
# set seed and load packages and functions
sim = 1 # load as environment variable for multiple runs
set.seed(sim) 
library(stats)
source("functions.R")

#-------------------------------------------------------------------------------
# Set parameters
alpha = 0.1 #confidence level
n = 800 #sample size
m = 3 #number of models

beta=0.6 #constantly biased forecaster
gam = 0.998 #improving forecaster
delta=0.008 #disimproving forecaster

# specify function g for generalized piece wise quantile loss
g =function(x){pnorm(x,sd=1)}

#-------------------------------------------------------------------------------
# Simulate means
y=rnorm(n)

#-------------------------------------------------------------------------------
# Compute losses
L = matrix(0,nrow=n,ncol=m) # to store the losses
L[,1]= 0.5*abs(g(y+beta)-g(y))
L[,2]= 0.5*abs(g(y+gam^(1:n))-g(y))
L[,3]= 0.5*abs(g(y+delta*(1:n))-g(y))

#-------------------------------------------------------------------------------
# Set universal c and lambda
c= 1
lambda = 1/(1.1*c)
psi_E_c = (-log(1-c*lambda)-c*lambda)/c^2

#-------------------------------------------------------------------------------
# Preparing processes
d = Delta_hat= V =array(0, c(m,m,n)) 
#' arrays to store the relative score differences,
#' the accumulated losses and the variance processes

for (i in 1:m){
  for (j in (1:m)[-i]){
    d[i,j,]= L[,i]- L[,j]
    V[i,j,] = cumsum((d[i,j,]-cumsum(c(0,d[i,j,-n]))/c(1,1:(n-1)))^2)
    Delta_hat[i,j,]=cumsum(d[i,j,])
  }
}
Delta_hat = list(Delta_hat)
V = list(V)
names(Delta_hat)[1]=names(V)[1]=paste0(1:m,collapse=",")

#-------------------------------------------------------------------------------
# Compute SMCSs by Lemma 3.5.
MCS= rep(list(integer(0)),n)
MCS[[1]]=1:m
K= m^2-m

for (t in 2:n){
  models = 1:m
  rej=rep(NA,K)
  func = function(x){ #x is the vector transform of a 
    # matrix X of dimension mxm without diagonal elements
    return(mean(exp(lambda*(matrix_to_vector(Delta_hat[[1]][,,t]))-lambda*t*x-
                             psi_E_c*matrix_to_vector(V[[1]][,,t]))))}
  for (i in 1:K){  
    upb = rep(c/2,K)
    upb[i]=0.001 # set the constraint x_i=0
    starting_point = rep(c/4,K)
    X=nlminb(starting_point,objective = func,lower=rep(0,K),upper=upb)
    #minimize function under nonlinear constraints
    rej[i]= (X$objective>1/alpha)
  }
  rej=rowSums(vector_to_matrix(rej))>0
  models = models[which(rej==0)]
 
  MCS[[t]]= models
}

#-------------------------------------------------------------------------------
# Compute superior objects
Mstar= rep(list(integer(0)),n)
freq = size = rep(NA,n) #coverage freq. of sup. objects and size of SMCSs

EL = L #expected losses
EL[,1] = cumsum(rep(pnorm(beta/sqrt(2)),n))
EL[,2] = cumsum(pnorm(gam^(1:n)/sqrt(2)))
EL[,3] = cumsum(pnorm(delta*(1:n)/sqrt(2)))
Mstar = apply(EL,1,which.min)
Mstar = as.list(Mstar) #superior objects

#-------------------------------------------------------------------------------
# Compute coverage freq. and sizes of superior objects
for (t in 1:n){
  size[t] = length(MCS[[t]])
  freq[t] = is.element(Mstar[[t]],MCS[[t]])
}

export = list("freq"=freq,"size"=size)
save(export, file=paste0("Simulation=",sim,".rda"))




