source("functions.R")
library(stats)

#-------------------------------------------------------------------------------
# load environment variables and packages
sim <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(sim)


alpha = 0.1
# Time
n = 800

# Fix parameters
alph = 0.6
beta=0.998
gam = 0.008

# specifying function g
g =function(x){pnorm(x,sd=1)}


#-------------------------------------------------------------------------------
# Simulate means
mu=rnorm(n)
#mu[1] = rnorm(1)
#for (t in 2:n){ mu[t]=rnorm(1,mean=mu[t-1])}
  
# Prepare objects to store
m = 3
L = matrix(0,nrow=n,ncol=m) # to store the losses
L[,1]= 0.5*abs(g(mu+alph)-g(mu))
L[,2]= 0.5*abs(g(mu+beta^(1:n))-g(mu))
L[,3]= 0.5*abs(g(mu+gam*(1:n))-g(mu))

#LL = matrix(0,nrow=n,ncol=m) # to store the losses
#LL[,1]= g(gam)
#LL[,2]= g(alph^(1:n))
#LL[,3]= g(beta*(1:n))

#-------------------------------------------------------------------------------
# Set c and lambda
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
# Compute model confidence sets
MCS= rep(list(integer(0)),n)
MCS[[1]]=1:m
for (t in 2:n){
  m_tmp = m
  K_tmp = m^2-m
  models = 1:m
  rej=rep(NA,K_tmp)
  func = function(x){ #x is the vector transform of a 
    # matrix X of dimension mxm without diagonal elements
    return(mean(exp(lambda*(matrix_to_vector(Delta_hat[[1]][,,t]))-lambda*t*x-
                             psi_E_c*matrix_to_vector(V[[1]][,,t]))))}
  for (i in 1:K_tmp){  
    upb = rep(c/2,K)
    upb[i]=0.001 # set the constraint x_i=0
    starting_point = rep(c/4,K)
    X=nlminb(starting_point,objective = func,lower=rep(0,K),upper=upb)
    rej[i]= (X$objective>1/alpha)
  }
  rej=rowSums(vector_to_matrix(rej))>0
  models = models[which(rej==0)]
 
  MCS[[t]]= models
}
#-------------------------------------------------------------------------------
# Superior objects
Mstar= rep(list(integer(0)),n)
freq = size = rep(NA,n)
tmp = matrix(NA,3,n)

EL = L
sigma = 1
EL[,1] = cumsum(rep(pnorm(alph/sqrt(1+sigma^2)),n))
EL[,2] = cumsum(pnorm(beta^(1:n)/sqrt(1+sigma^2)))
EL[,3] = cumsum(pnorm(gam*(1:n)/sqrt(1+sigma^2)))
Mstar = apply(EL,1,which.min)
Mstar = as.list(Mstar)


for (t in 1:n){
  size[t] = length(MCS[[t]])
  freq[t] = is.element(Mstar[[t]],MCS[[t]])
}

export = list("freq"=freq,"size"=size)
save(export, file=paste0("Simulation=",sim,".rda"))




