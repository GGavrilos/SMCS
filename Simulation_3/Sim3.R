######################################
### One single run of Simulation 3 ###
######################################



source("functions.R")
library(RColorBrewer)
library(pracma)
library(NlcOptim)
set.seed(1)


## Random case
set.seed(1)
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

  
# Prepare objects to store
m = 3
L = matrix(0,nrow=n,ncol=m) # to store the losses
L[,1]= 0.5*abs(g(mu+alph)-g(mu))
L[,2]= 0.5*abs(g(mu+beta^(1:n))-g(mu))
L[,3]= 0.5*abs(g(mu+gam*(1:n))-g(mu))


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
    upb=rep(c/2,K_tmp)
    upb[i]=0.0001 # set the constraint x_i=0
    rej[i]= (fmincon(rep(c/4,K_tmp),fn=func,lb=rep(0,K_tmp),ub=upb)$value>1/alpha)
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
  tmp[1,t] =  is.element(1,MCS[[t]])
  tmp[2,t] =  is.element(2,MCS[[t]])
  tmp[3,t] =  is.element(3,MCS[[t]])
}

t1=min(which(unlist(Mstar)==1))
t2=min(which(unlist(Mstar)==2))

export = list("MCS"=tmp,"L"=L)
save(export,file="MCS_one_simulation.rda")



