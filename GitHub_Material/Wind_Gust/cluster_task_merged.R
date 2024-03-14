######################################################################
############### Load packages and functions ##########################
######################################################################
library(crch) # ptlogis: cdf of the truncated logistic distribution 
library(rootSolve) # uniroot.all: find all roots on an interval
library(scoringRules) # for computation of the crps
library(pracma) # fmincon: minimizing function under condition
library(stats)
library(lubridate)
source("functions.R")


######################################################################
########################## Choose stations ###########################
######################################################################
load("scores_ens_hr0_step18.RData")

# Consider only locations with at least n=2600 obs
loc_vec <- names(table(df_scores$location)[table(df_scores$location) >= 2600])
K = length(loc_vec)

# Consider all initialization times
df_sub <- subset(df_scores, is.element(location, loc_vec))
init_vec <- unique(df_sub$init_tm)
n = length(init_vec)

######################################################################
#### load scaled loss differences and times of the chosen stations ###
######################################################################
losses = times=list(0)

for (k in 1:K){
  load(paste0("loss_bounds_",loc_vec[k],".rda"))
  losses[[k]]= list1$loss_differences
  times[[k]] = list1$times
}

######################################################################
################ Compute merged score differences ####################
######################################################################
m = 9
d = array(0,dim=c(m,m,n))
for (t in 1:n){ # t = time
  tmp=rep(NA,K)
  for (k in 1:K){ # k = station
    tmp[k]=is.element(init_vec[t],times[[k]])
    if (tmp[k]==1){
      l = which(init_vec[t]==times[[k]])
      d[,,t]=d[,,t]+losses[[k]][,,l]
      }
  }
  d[,,t]=d[,,t]/sum(tmp)
}


######################################################################
####################### Prepare all processes ########################
######################################################################

Delta_hat= V =array(0, c(m,m,n)) 
#' arrays to store the accumulated losses and the variance processes

for (i in 1:m){
  for (j in (1:m)[-i]){
    V[i,j,] = cumsum((d[i,j,]-cumsum(c(0,d[i,j,-n]))/c(1,1:(n-1)))^2)
    Delta_hat[i,j,]=cumsum(d[i,j,])
  }
}

#' Since we work with the rescaled loss differences, we can choose a 
#' constant lambda-value.
C=2
lambda = 1/(2*C)
psi_E_c = (-log(1-C*lambda)-C*lambda)/C^2

######################################################################
########################## Compute the MCS ###########################
######################################################################
alpha = 0.1 # confidence level

MCS= rep(list(integer(0)),n)
MCS[[1]]=1:m
K = m^2-m
for (t in 2:n){
  if (sum(is.na(matrix_to_vector(Delta_hat[,,t])))==0){
  models = 1:m
  rej=rep(NA,K)
  func = function(x){ #x is the vector transform of a 
    # matrix X of dimension mxm without diagonal elements
    return(mean(exp(lambda*(matrix_to_vector(Delta_hat[,,t]))-lambda*t*x-
                      psi_E_c*matrix_to_vector(V[,,t]))))}
  for (i in 1:K){  
    upb = rep(C/2,K)
    upb[i]=0.001 # set the constraint x_i=0
    starting_point = rep(C/4,K)
    starting_point[i]=0.001 # set the constraint x_i=0
    rej[i]= (fmincon(rep(C/4,K),fn=func,lb=rep(0,K),ub=upb)$value>1/alpha)
    #solnl(X=rep(c/4,m-1),objfun=f,,lb=rep(0,m-1),ub=rep(c/2,m-1))
    X=nlminb(starting_point,objective = func,lower=rep(0,K),upper=upb)
    rej[i]= (X$objective>1/alpha)
  }
  rej=rowSums(vector_to_matrix(rej))>0
  models = models[which(rej==0)]
  
  MCS[[t]]= models
  print(t)
  }
}


######################################################################
########################### Export files #############################
######################################################################
size = rep(NA,n)
elements = matrix(NA,n,m)
colnames(elements) = c("ens","emos","mbm","idr","emos_gb","qrf","drn","bqn","hen")
rownames(elements) = names(size)=names(MCS)=as.character(init_vec)
for (t in 1:n){
  if(length(MCS[[t]])>0){
  size[t] = length(MCS[[t]])
  }
  for (i in 1:m){ 
    if(!is.na(size[t])){
    elements[t,i] =  is.element(i,MCS[[t]])
    }
  }
}

export=list("size"=size,"elements"=elements,"MCS"=MCS,"times"=init_vec,"losses"=d)
save(export,file="merged_results.rda")
