######################################################################
############### Load packages and functions ##########################
######################################################################
library(crch) #ptlogis: cdf of the truncated logistic distribution 
library(rootSolve) #uniroot.all: find all roots on an interval
library(scoringRules) #for computation of the crps
library(stats) #nlminb: minimizing function under nonlinear constraints
library(lubridate)
source("functions.R")

######################################################################
########################## Select a station ##########################
######################################################################
load("scores_ens_hr0_step18.RData") 
station_id = 1 # load as environment variable in 1:174
station= (unique(df_scores$location))[station_id] 
data_ens= subset(df_scores,location==station)
data_ens=data_ens[order(decimal_date(data_ens$init_tm)),]

######################################################################
################ Load bounds and loss differences ####################
######################################################################
load(paste0("loss_bounds_",station,".rda"))
d= list1$loss_differences
c= list1$bounds
n = dim(d)[3]
m = dim(d)[1]

######################################################################
######################## Prepare all processes #######################
######################################################################

Delta_hat= V =array(0, c(m,m,n)) 
# store the accumulated losses and the variance processes
for (i in 1:m){
  for (j in (1:m)[-i]){
    V[i,j,] = cumsum((d[i,j,]-cumsum(c(0,d[i,j,-n]))/c(1,1:(n-1)))^2)
    Delta_hat[i,j,]=cumsum(d[i,j,])
  }
}

C=2 # Set uniform c and lambda
lambda = 1/(2*C)
psi_E_c = (-log(1-C*lambda)-C*lambda)/C^2

######################################################################
################# Compute the SMCS by Lemma 3.5. #####################
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
    X=nlminb(starting_point,objective = func,lower=rep(0,K),upper=upb)
    rej[i]= (X$objective>1/alpha)
     }
  rej=rowSums(vector_to_matrix(rej))>0
  models = models[which(rej==0)]
  
  MCS[[t]]= models
  }
}

######################################################################
########################### Export files #############################
######################################################################
size = rep(NA,n) #size of SMCSs
elements = matrix(NA,n,m)
colnames(elements) = c("ens","emos","mbm","idr","emos_gb","qrf","drn","bqn","hen")
rownames(elements) = names(size)=names(MCS)=as.character(data_ens$init_tm)
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

times=data_ens$init_tm

list2=list("size"=size,"elements"=elements,"MCS"=MCS,"times"=times)
save(list2,file=paste0("station=",station,".rda"))
