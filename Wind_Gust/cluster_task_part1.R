######################################################################
############### Load packages and functions ##########################
######################################################################
library(crch) # ptlogis: cdf of the truncated logistic distribution 
library(rootSolve) # uniroot.all: find all roots on an interval
library(scoringRules) # for computation of the crps
library(pracma) # fmincon: minimizing function under condition
library(lubridate)
source("functions.R")
source("fn_eval.R")

######################################################################
########################## Select a station ##########################
######################################################################
load("scores_ens_hr0_step18.RData") 
station_id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#station_id = 2
station= (unique(df_scores$location))[station_id] 

######################################################################
############### load the forecasts for all PP-methods ################
######################################################################
load("scores_ens_hr0_step18.RData")
data_ens= subset(df_scores,location==station)
data_ens=data_ens[order(decimal_date(data_ens$init_tm)),] 
#in case that days are not properly ordered
ens = as.matrix(data_ens[,which(is.element(colnames(data_ens),paste0("ens_", 1:20)))]) # dataframe with ensemble of size 20 per day

load("scores_emos_sea_hr0_step18.RData")
data_emos= subset(df_scores,location==station)
data_emos=data_emos[order(decimal_date(data_emos$init_tm)),] 
#in case that days are not properly ordered
loc_emos = data_emos$loc # location of trun logis distr per day
scale_emos = data_emos$scale # scale of trun logis distr per day

load("scores_mbm_sea_hr0_step18.RData")
data_mbm= subset(df_scores,location==station)
data_mbm=data_mbm[order(decimal_date(data_mbm$init_tm)),] 
#in case that days are not properly ordered
ens_mbm = as.matrix(data_mbm[,which(is.element(colnames(data_mbm),paste0("ens_", 1:20)))]) # dataframe with ensemble of size 20 per day

load("scores_idr_hr0_step18.RData")
data_idr= subset(df_scores,location==station)
data_idr=data_idr[order(decimal_date(data_idr$init_tm)),] 
#in case that days are not properly ordered
idr_quant=as.matrix(data_idr[,which(is.element(colnames(data_idr),paste0("q_", 1:125)))])
# quantile predictions at 125 equidistant quantile levels per day
ls_idr=ls_idr[[which(names(ls_idr)==station)]]

load("scores_emos_bst_hr0_step18.RData")
data_emos_gb= subset(df_scores,location==station)
data_emos_gb=data_emos_gb[order(decimal_date(data_emos_gb$init_tm)),] 
#in case that days are not properly ordered
loc_emos_gb = data_emos_gb$loc # location of trun logis distr per day
scale_emos_gb = data_emos_gb$scale # scale of trun logis distr per day

load("scores_qrf_loc_hr0_step18.RData")
data_qrf= subset(df_scores,location==station)
data_qrf=data_qrf[order(decimal_date(data_qrf$init_tm)),] 
#in case that days are not properly ordered
qrf_quant=as.matrix(data_qrf[,which(is.element(colnames(data_qrf),paste0("q_", 1:125)))])
# quantile predictions at 125 equidistant quantile levels per day

load("scores_drn_hr0_step18.RData")
data_drn= subset(df_scores,location==station)
data_drn=data_drn[order(decimal_date(data_drn$init_tm)),] 
#in case that days are not properly ordered
loc_drn = data_drn$loc # location of trun logis distr per day
scale_drn = data_drn$scale # scale of trun logis distr per day

load("scores_bqn_hr0_step18.RData")
data_bqn= subset(df_scores,location==station)
data_bqn=data_bqn[order(decimal_date(data_bqn$init_tm)),] 
#in case that days are not properly ordered
bqn_quant=as.matrix(data_bqn[,which(is.element(colnames(data_bqn),paste0("q_", 1:125)))])
# quantile predictions at 125 equidistant quantile levels per day

load("scores_hen_hr0_step18.RData")
N = dim(df_scores)[1]
rownames(df_scores)=1:N
data_hen= subset(df_scores,location==station)
data_hen=data_hen[order(decimal_date(data_hen$init_tm)),] 
#in case that days are not properly ordered
days_hen=as.numeric(rownames(data_hen))
hen_mas=(ls_f$f)[days_hen] 
# probability masses of the piecewise linear hen forecasts
hen_jumps=(ls_f$bin_edges_f)[days_hen]
# probability masses of the piecewise linear hen forecasts

######################################################################
######################## General quantities ##########################
######################################################################
n = length(days_hen) 
# number of data points are the same for each PP-method
obs = data_ens$obs #observations
m = 9 # number of models: 
#ens(1),emos(2),mbm(3),idr(4),emos_gb(5),qrf(6),drn(7),bqn(8),hen(9)

######################################################################
################## Compute the predictable bounds ####################
######################################################################
c = array(NA,dim=c(m,m,n)) # to store the predictable bounds

lob=0
upb=max(obs)+mean(obs)/2 # upper bound to check for crossing points
high = 1e10

#emos-emos_gb
for (i in 1:n){
func= function(x){
  return(ptlogis(x,location = loc_emos[i],scale=scale_emos[i],left=0)
  -ptlogis(x,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0))
}
tmp1= uniroot.all(func,lower=0,upper=upb)
n_ind = 2+length(tmp1)
c[2,5,i]=c[5,2,i] = max(abs(c(
  crps_tlogis(c(0,high,tmp1),location=loc_emos[i],scale=scale_emos[i],lower=0)
  -crps_tlogis(c(0,high,tmp1),location=loc_emos_gb[i],scale=scale_emos_gb[i],lower=0))))

}
#emos-drn
for (i in 1:n){
  func= function(x){
    return(ptlogis(x,location = loc_emos[i],scale=scale_emos[i],left=0)
           -ptlogis(x,location = loc_drn[i],scale=scale_drn[i],left=0))
  }
  tmp1= uniroot.all(func,lower=0,upper=upb)
  n_ind = 2+length(tmp1)
  c[2,7,i]=c[7,2,i]=max(abs(c(crps_tlogis(c(0,high,tmp1),location=loc_emos[i],scale=scale_emos[i],lower=0)
               -crps_tlogis(c(0,high,tmp1),location=loc_drn[i],scale=scale_drn[i],lower=0))))
          
}
#emos_gb-drn
for (i in 1:n){
  func= function(x){
    return(ptlogis(x,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0)
           -ptlogis(x,location = loc_drn[i],scale=scale_drn[i],left=0))
  }
  tmp1= uniroot.all(func,lower=0,upper=upb)
  tmp1= uniroot.all(func,lower=0,upper=upb)
  n_ind = 2+length(tmp1)
  c[5,7,i]=c[7,5,i]=max(abs(c(crps_tlogis(c(0,high,tmp1),location=loc_emos_gb[i],scale=scale_emos_gb[i],lower=0)
                              -crps_tlogis(c(0,high,tmp1),location=loc_drn[i],scale=scale_drn[i],lower=0))))

}
#ens-mbm
for (i in 1:n){
  #func=function(x){ecdf(ens[i,])(x)-ecdf(ens_mbm[i,])(x)}
  #tmp1= uniroot.all(func,lower=0,upper=upb)
  thresholds= sort(c(ens[i,],ens_mbm[i,]))
  ind = which(ecdf(ens[i,])(thresholds)==ecdf(ens_mbm[i,])(thresholds))
  if (length(ind)>0){ind= c(ind,ind[-length(ind)]+1)}
  n_ind=length(ind)+2
  c[1,3,i]=c[3,1,i]=
    max(abs(c(
      crps_sample(y=c(0,high,thresholds[ind]), dat=matrix(rep(ens[i,],n_ind),nrow= n_ind,byrow=TRUE))
  -crps_sample(y=c(0,high,thresholds[ind]),dat=matrix(rep(ens_mbm[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#ens-emos
for (i in 1:n){
  func=function(x){ecdf(ens[i,])(x)-
      ptlogis(x,location = loc_emos[i],scale=scale_emos[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(ens[i,]))
  n_ind = 2+length(tmp1)
  c[1,2,i]=c[2,1,i]=
  max(abs(c(
    crps_tlogis(c(0,high,tmp1),location=loc_emos[i],scale=scale_emos[i],lower=0)
  - crps_sample(y=c(0,high,tmp1), dat=matrix(rep(ens[i,],n_ind),nrow= n_ind,byrow=TRUE)))))
           
}
#ens-emos_gb
for (i in 1:n){
  func=function(x){ecdf(ens[i,])(x)-
      ptlogis(x,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(ens[i,]))
  n_ind = 2+length(tmp1)
  c[1,5,i]=c[5,1,i]=
  max(abs(c(
    crps_tlogis(c(0,high,tmp1),location=loc_emos_gb[i],scale=scale_emos_gb[i],lower=0)
    - crps_sample(y=c(0,high,tmp1), dat=matrix(rep(ens[i,],n_ind),nrow= n_ind,byrow=TRUE)))))
}
#ens-drn
for (i in 1:n){
  func=function(x){ecdf(ens[i,])(x)-
      ptlogis(x,location = loc_drn[i],scale=scale_drn[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(ens[i,]))
  n_ind = 2+length(tmp1)
  c[1,7,i]=c[7,1,i]=
    max(abs(c(
      crps_tlogis(c(0,high,tmp1),location=loc_drn[i],scale=scale_drn[i],lower=0)
      - crps_sample(y=c(0,high,tmp1), dat=matrix(rep(ens[i,],n_ind),nrow= n_ind,byrow=TRUE)))))
}
#mbm-emos
for (i in 1:n){
  func=function(x){ecdf(ens_mbm[i,])(x)-
      ptlogis(x,location = loc_emos[i],scale=scale_emos[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(ens_mbm[i,]))
  n_ind = 2+length(tmp1)
  c[3,2,i]=c[2,3,i]=
    max(abs(c(
      crps_tlogis(c(0,high,tmp1),location=loc_emos[i],scale=scale_emos[i],lower=0)
      - crps_sample(y=c(0,high,tmp1), dat=matrix(rep(ens_mbm[i,],n_ind),nrow= n_ind,byrow=TRUE)))))
}
#mbm-emos_gb
for (i in 1:n){
  func=function(x){ecdf(ens_mbm[i,])(x)-
      ptlogis(x,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(ens_mbm[i,]))
  n_ind = 2+length(tmp1)
  c[3,5,i]=c[5,3,i]=
    max(abs(c(
      crps_tlogis(c(0,high,tmp1),location=loc_emos_gb[i],scale=scale_emos_gb[i],lower=0)
      - crps_sample(y=c(0,high,tmp1), dat=matrix(rep(ens_mbm[i,],n_ind),nrow= n_ind,byrow=TRUE)))))
}
#mbm-drn
for (i in 1:n){
  func=function(x){ecdf(ens_mbm[i,])(x)-
      ptlogis(x,location = loc_drn[i],scale=scale_drn[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(ens_mbm[i,]))
  n_ind = 2+length(tmp1)
  c[3,7,i]=c[7,3,i]= max(abs(c(
    crps_tlogis(c(0,high,tmp1),location=loc_drn[i],scale=scale_drn[i],lower=0)
    - crps_sample(y=c(0,high,tmp1), dat=matrix(rep(ens_mbm[i,],n_ind),nrow= n_ind,byrow=TRUE)))))
}
#idr-qrf
for (i in 1:n){
  thresholds= unique(sort(c(idr_quant[i,],qrf_quant[i,])))
  ind = which(diff(sign(idr_func(thresholds,ls_idr[[i]])-ecdf(qrf_quant[i,])(thresholds)))!=0)+1
  n_ind=length(ind)+2
  ls_tmp=rep(list(ls_idr[[i]]),n_ind)
  attributes(ls_tmp)=  attributes(ls_idr)
  c[4,6,i]=c[6,4,i]=
  max(abs(c(isodistrreg:::crps(ls_tmp,y=c(0,high,thresholds[ind]))
  -crps_sample(y=c(0,high,thresholds[ind]),dat=matrix(rep(qrf_quant[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#idr-bqn
for (i in 1:n){
  thresholds= unique(sort(c(idr_quant[i,],bqn_quant[i,])))
  ind = which(diff(sign(idr_func(thresholds,ls_idr[[i]])-ecdf(bqn_quant[i,])(thresholds)))!=0)+1
  n_ind=length(ind)+2
  ls_tmp=rep(list(ls_idr[[i]]),n_ind)
  attributes(ls_tmp)= attributes(ls_idr)
  c[4,8,i]=c[8,4,i]=
    max(abs(c(isodistrreg:::crps(ls_tmp,y=c(0,high,thresholds[ind]))
    -crps_sample(y=c(0,high,thresholds[ind]),dat=matrix(rep(bqn_quant[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#bqn-qrf
for (i in 1:n){
  thresholds= unique(sort(c(bqn_quant[i,],qrf_quant[i,])))
  ind = which(diff(sign(ecdf(bqn_quant[i,])(thresholds)-ecdf(qrf_quant[i,])(thresholds)))!=0)+1
  n_ind=length(ind)+2
  c[8,6,i]=c[6,8,i]=
    max(abs(c(crps_sample(y=c(0,high,thresholds[ind]),
                    dat=matrix(rep(bqn_quant[i,],n_ind),nrow= n_ind,byrow=TRUE))
        -crps_sample(y=c(0,high,thresholds[ind]),
                     dat=matrix(rep(qrf_quant[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#idr-ens
for (i in 1:n){
  thresholds= unique(sort(c(idr_quant[i,],ens[i,])))
  ind = which(diff(sign(idr_func(thresholds,ls_idr[[i]])-ecdf(ens[i,])(thresholds)))!=0)+1
  n_ind=length(ind)+2
  ls_tmp=rep(list(ls_idr[[i]]),n_ind)
  attributes(ls_tmp)= attributes(ls_idr)
  c[4,1,i]=c[1,4,i]=
    max(abs(c(isodistrreg:::crps(ls_tmp,y=c(0,high,thresholds[ind]))
        -crps_sample(y=c(0,high,thresholds[ind]),
                     dat=matrix(rep(ens[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#qrf-ens
for (i in 1:n){
  thresholds= unique(sort(c(qrf_quant[i,],ens[i,])))
  ind = which(diff(sign(ecdf(qrf_quant[i,])(thresholds)-ecdf(ens[i,])(thresholds)))!=0)+1
  n_ind=length(ind)+2
  c[6,1,i]=c[1,6,i]=
    max(abs(c(crps_sample(y=c(0,high,thresholds[ind]),
                    dat=matrix(rep(qrf_quant[i,],n_ind),nrow= n_ind,byrow=TRUE))
        -crps_sample(y=c(0,high,thresholds[ind]),
                     dat=matrix(rep(ens[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#bqn-ens
for (i in 1:n){
  thresholds= unique(sort(c(bqn_quant[i,],ens[i,])))
  ind = which(diff(sign(ecdf(bqn_quant[i,])(thresholds)-ecdf(ens[i,])(thresholds)))!=0)+1
  n_ind=length(ind)+2
  c[8,1,i]=c[1,8,i]=
    max(abs(c(crps_sample(y=c(0,high,thresholds[ind]),
                    dat=matrix(rep(bqn_quant[i,],n_ind),nrow= n_ind,byrow=TRUE))
        -crps_sample(y=c(0,high,thresholds[ind]),
                     dat=matrix(rep(ens[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#idr-mbm
for (i in 1:n){
  thresholds= unique(sort(c(idr_quant[i,],ens_mbm[i,])))
  ind = which(diff(sign(idr_func(thresholds,ls_idr[[i]])-ecdf(ens_mbm[i,])(thresholds)))!=0)+1
  n_ind=length(ind)+2
  ls_tmp=rep(list(ls_idr[[i]]),n_ind)
  attributes(ls_tmp)= attributes(ls_idr)
  c[4,3,i]=c[3,4,i]=
    max(abs(c(isodistrreg:::crps(ls_tmp,y=c(0,high,thresholds[ind]))
        -crps_sample(y=c(0,high,thresholds[ind]),
                     dat=matrix(rep(ens_mbm[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#qrf-mbm
for (i in 1:n){
  thresholds= unique(sort(c(qrf_quant[i,],ens_mbm[i,])))
  ind = which(diff(sign(ecdf(qrf_quant[i,])(thresholds)-ecdf(ens_mbm[i,])(thresholds)))!=0)+1
  n_ind=length(ind)+2
  c[6,3,i]=c[3,6,i]=
    max(abs(c(crps_sample(y=c(0,high,thresholds[ind]),
                    dat=matrix(rep(qrf_quant[i,],n_ind),nrow= n_ind,byrow=TRUE))
        -crps_sample(y=c(0,high,thresholds[ind]),
                     dat=matrix(rep(ens_mbm[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#bqn-mbm
for (i in 1:n){
  thresholds= unique(sort(c(bqn_quant[i,],ens_mbm[i,])))
  ind = which(diff(sign(ecdf(bqn_quant[i,])(thresholds)-ecdf(ens_mbm[i,])(thresholds)))!=0)+1
  n_ind=length(ind)+2
  c[8,3,i]=c[3,8,i]=
    max(abs(c(crps_sample(y=c(0,high,thresholds[ind]),
                    dat=matrix(rep(bqn_quant[i,],n_ind),nrow= n_ind,byrow=TRUE))
        -crps_sample(y=c(0,high,thresholds[ind]),
                     dat=matrix(rep(ens_mbm[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#idr-emos
for (i in 1:n){
  #func=function(x){ecdf(idr_quant[i,])(x)-ptlogis(x,location = loc_emos[i],scale=scale_emos[i],left=0)}
  func=function(x){idr_func(x,ls_idr[[i]])-ptlogis(x,location = loc_emos[i],scale=scale_emos[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=upb)
  n_ind = length(tmp1)+2
  ls_tmp=rep(list(ls_idr[[i]]),n_ind)
  attributes(ls_tmp)= attributes(ls_idr)
  c[4,2,i]=c[2,4,i]=
    max(abs(c(crps_tlogis(c(0,high,tmp1),location=loc_emos[i],scale=scale_emos[i],lower=0)
                              -isodistrreg:::crps(ls_tmp,y=c(0,high,tmp1)))))
  
}
#idr-drn
for (i in 1:n){
  #func=function(x){ecdf(idr_quant[i,])(x)-ptlogis(x,location = loc_drn[i],scale=scale_drn[i],left=0)}
  func=function(x){idr_func(x,ls_idr[[i]])-ptlogis(x,location = loc_drn[i],scale=scale_drn[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(idr_quant[i,]))
  n_ind = length(tmp1)+2
  ls_tmp=rep(list(ls_idr[[i]]),n_ind)
  attributes(ls_tmp)= attributes(ls_idr)
  c[4,7,i]=c[7,4,i]=max(abs(c(crps_tlogis(c(0,high,tmp1),location=loc_drn[i],scale=scale_drn[i],lower=0)
                              -isodistrreg:::crps(ls_tmp,y=c(0,high,tmp1)))))
}
#idr-emos_gb
for (i in 1:n){
  #func=function(x){ecdf(idr_quant[i,])(x)-ptlogis(x,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0)}
  func=function(x){idr_func(x,ls_idr[[i]])-ptlogis(x,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(idr_quant[i,]))
  n_ind = length(tmp1)+2
  ls_tmp=rep(list(ls_idr[[i]]),n_ind)
  attributes(ls_tmp)= attributes(ls_idr)
  c[4,5,i]=c[5,4,i]=max(abs(c(crps_tlogis(c(0,high,tmp1),location=loc_emos_gb[i],scale=scale_emos_gb[i],lower=0)
                              -isodistrreg:::crps(ls_tmp,y=c(0,high,tmp1)))))
}
#qrf-emos
for (i in 1:n){
  func=function(x){ecdf(qrf_quant[i,])(x)-
      ptlogis(x,location = loc_emos[i],scale=scale_emos[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(qrf_quant[i,]))
  n_ind = 2+length(tmp1)
  c[6,2,i]=c[2,6,i]=max(abs(c(
    crps_tlogis(c(0,high,tmp1),location=loc_emos[i],scale=scale_emos[i],lower=0)
    -crps_sample(y=c(0,high,tmp1),dat=matrix(rep(qrf_quant[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#qrf-drn
for (i in 1:n){
  func=function(x){ecdf(qrf_quant[i,])(x)-
      ptlogis(x,location = loc_drn[i],scale=scale_drn[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(qrf_quant[i,]))
  n_ind = 2+length(tmp1)
  c[6,7,i]=c[7,6,i]=max(abs(c(
    crps_tlogis(c(0,high,tmp1),location=loc_drn[i],scale=scale_drn[i],lower=0)
    -crps_sample(y=c(0,high,tmp1),dat=matrix(rep(qrf_quant[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#qrf-emos_gb
for (i in 1:n){
  func=function(x){ecdf(qrf_quant[i,])(x)-
      ptlogis(x,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(qrf_quant[i,]))
  n_ind = 2+length(tmp1)
  c[6,5,i]=c[5,6,i]=max(abs(c(
    crps_tlogis(c(0,high,tmp1),location=loc_emos_gb[i],scale=scale_emos_gb[i],lower=0)
    -crps_sample(y=c(0,high,tmp1),dat=matrix(rep(qrf_quant[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#bqn-emos
for (i in 1:n){
  func=function(x){ecdf(bqn_quant[i,])(x)-
      ptlogis(x,location = loc_emos[i],scale=scale_emos[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(bqn_quant[i,]))
  n_ind = 2+length(tmp1)
  c[8,2,i]=c[2,8,i]=max(abs(c(
    crps_tlogis(c(0,high,tmp1),location=loc_emos[i],scale=scale_emos[i],lower=0)
    -crps_sample(y=c(0,high,tmp1),dat=matrix(rep(bqn_quant[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#bqn-drn
for (i in 1:n){
  func=function(x){ecdf(bqn_quant[i,])(x)-
      ptlogis(x,location = loc_drn[i],scale=scale_drn[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(bqn_quant[i,]))
  n_ind = 2+length(tmp1)
  c[8,7,i]=c[7,8,i]=max(abs(c(
    crps_tlogis(c(0,high,tmp1),location=loc_drn[i],scale=scale_drn[i],lower=0)
    -crps_sample(y=c(0,high,tmp1),dat=matrix(rep(bqn_quant[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#bqn-emos_gb
for (i in 1:n){
  func=function(x){ecdf(bqn_quant[i,])(x)-
      ptlogis(x,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=max(bqn_quant[i,]))
  n_ind = 2+length(tmp1)
  c[8,5,i]=c[5,8,i]=max(abs(c(
    crps_tlogis(c(0,high,tmp1),location=loc_emos_gb[i],scale=scale_emos_gb[i],lower=0)
    -crps_sample(y=c(0,high,tmp1),dat=matrix(rep(bqn_quant[i,], n_ind),nrow= n_ind,byrow=TRUE)))))
}
#hen
for (i in 1:n){
# cdf of hen forecast
  hen_cdf=function(x){
    y = x
    for (j in 1:length(x)){
    if(x[j]<hen_jumps[[i]][1]){y[j]=0}
    else{tmp=max(which(hen_jumps[[i]]<=x[j]))
      if(tmp==length(hen_jumps[[i]])){y[j]=1}
      else{y[j]=sum(hen_mas[[i]][1:tmp])}  
      }
    }
    return(y)
  }
  
#hen-ens
  thresholds = unique(c(ens[i,],hen_jumps[[i]]))
  n_ind = length(thresholds)+2
  hen_mas_tmp = rep(list(hen_mas[[i]]),n_ind)
  hen_jumps_tmp = rep(list(hen_jumps[[i]]),n_ind)
  
  
  c[1,9,i]=c[9,1,i]=max(abs(
      crps_sample(c(0,high,thresholds),dat=matrix(rep(ens[i,], n_ind),nrow= n_ind,byrow=TRUE))-
      crps_hd(c(0,high,thresholds),f=hen_mas_tmp ,hen_jumps_tmp)))  

   
#hen-mbm
  thresholds = unique(c(ens_mbm[i,],hen_jumps[[i]]))
  n_ind = length(thresholds)+2
  hen_mas_tmp = rep(list(hen_mas[[i]]),n_ind)
  hen_jumps_tmp = rep(list(hen_jumps[[i]]),n_ind)
  
  c[3,9,i]=c[9,3,i]=max(abs(
    crps_sample(c(0,high,thresholds),dat=matrix(rep(ens_mbm[i,], n_ind),nrow= n_ind,byrow=TRUE))-
      crps_hd(c(0,high,thresholds),f=hen_mas_tmp ,hen_jumps_tmp)))  
  
 
   
#hen-idr
  thresholds = unique(c(ls_idr[[i]][,1],hen_jumps[[i]]))
  n_ind = length(thresholds)+2
  ls_tmp=rep(list(ls_idr[[i]]),n_ind)
  attributes(ls_tmp)= attributes(ls_idr)
  hen_mas_tmp = rep(list(hen_mas[[i]]),n_ind)
  hen_jumps_tmp = rep(list(hen_jumps[[i]]),n_ind)
  
  c[4,9,i]=c[9,4,i]= max(abs(c(crps_hd(c(0,high,thresholds),f=hen_mas_tmp ,hen_jumps_tmp)
                          -isodistrreg:::crps(ls_tmp,y=c(0,high,thresholds)))))
  
   
  #hen-qrf
  thresholds = unique(c(qrf_quant[i,],hen_jumps[[i]]))
  n_ind = length(thresholds)+2
  hen_mas_tmp = rep(list(hen_mas[[i]]),n_ind)
  hen_jumps_tmp = rep(list(hen_jumps[[i]]),n_ind)
  
  c[6,9,i]=c[9,6,i]=max(abs(
    crps_sample(c(0,high,thresholds),dat=matrix(rep(qrf_quant[i,], n_ind),nrow= n_ind,byrow=TRUE))-
      crps_hd(c(0,high,thresholds),f=hen_mas_tmp ,hen_jumps_tmp)))  
  
  
    
  #hen-bqn
  thresholds = unique(c(bqn_quant[i,],hen_jumps[[i]]))
  n_ind = length(thresholds)+2
  hen_mas_tmp = rep(list(hen_mas[[i]]),n_ind)
  hen_jumps_tmp = rep(list(hen_jumps[[i]]),n_ind)
  
  c[8,9,i]=c[9,8,i]=max(abs(
    crps_sample(c(0,high,thresholds),dat=matrix(rep(bqn_quant[i,], n_ind),nrow= n_ind,byrow=TRUE))-
      crps_hd(c(0,high,thresholds),f=hen_mas_tmp ,hen_jumps_tmp)))  
  
   
  #hen-emos
  func=function(x){hen_cdf(x)-
      ptlogis(x,location = loc_emos[i],scale=scale_emos[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=qtlogis(0.99,location = loc_emos[i],scale=scale_emos[i],left=0))
  n_ind = length(tmp1)+2
  hen_mas_tmp = rep(list(hen_mas[[i]]),n_ind)
  hen_jumps_tmp = rep(list(hen_jumps[[i]]),n_ind)
  
  c[9,2,i]=c[2,9,i]=max(abs(
    crps_tlogis(c(0,high,tmp1),location=loc_emos[i],scale=scale_emos[i],lower=0)-
    crps_hd(c(0,high,tmp1),f=hen_mas_tmp ,hen_jumps_tmp)))  
  
  #hen-emos-gb
  func=function(x){hen_cdf(x)-
      ptlogis(x,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=qtlogis(0.99,location = loc_emos_gb[i],scale=scale_emos_gb[i],left=0))
  n_ind = length(tmp1)+2
  hen_mas_tmp = rep(list(hen_mas[[i]]),n_ind)
  hen_jumps_tmp = rep(list(hen_jumps[[i]]),n_ind)
  
  c[9,5,i]=c[5,9,i]=max(abs(
    crps_tlogis(c(0,high,tmp1),location=loc_emos_gb[i],scale=scale_emos_gb[i],lower=0)-
      crps_hd(c(0,high,tmp1),f=hen_mas_tmp ,hen_jumps_tmp)))  
  
  
  #hen-drn
  func=function(x){hen_cdf(x)-
      ptlogis(x,location = loc_drn[i],scale=scale_drn[i],left=0)}
  tmp1= uniroot.all(func,lower=0,upper=qtlogis(0.99,location = loc_drn[i],scale=scale_drn[i],left=0))
  n_ind = length(tmp1)+2
  hen_mas_tmp = rep(list(hen_mas[[i]]),n_ind)
  hen_jumps_tmp = rep(list(hen_jumps[[i]]),n_ind)
  
  c[9,7,i]=c[7,9,i]=max(abs(
    crps_tlogis(c(0,high,tmp1),location=loc_drn[i],scale=scale_drn[i],lower=0)-
      crps_hd(c(0,high,tmp1),f=hen_mas_tmp ,hen_jumps_tmp)))  
  
 }


tmp = which(abs(c)<0.001,arr.ind=TRUE)
kk=nrow(tmp)
if (kk>0){
  for (i in 1:kk){
    c[tmp[i,1],tmp[i,2],tmp[i,3]] = 0.01
  }
}
######################################################################
######################## Prepare all processes #######################
######################################################################

L = matrix(0,nrow=n,ncol=m) # to store the CRPS values
L[,1]= data_ens$crps
L[,2]= data_emos$crps
L[,3]= data_mbm$crps
L[,4]= data_idr$crps
L[,5]= data_emos_gb$crps
L[,6]= data_qrf$crps
L[,7]= data_drn$crps
L[,8]= data_bqn$crps
L[,9]= data_hen$crps

d =array(0, c(m,m,n)) # array to store the SCALED score differences,

for (i in 1:m){
  for (j in (1:m)[-i]){
    d[i,j,]= (L[,i]- L[,j])/c[i,j,]
  }
}

times=data_ens$init_tm
list1=list("loss_differences"=d,"bounds"=c,"times"=times)
save(list1,file=paste0("loss_bounds_",station,".rda"))