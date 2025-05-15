#-------------------------------------------------------------------------------
## Adjusting by the closure principle (see Section 5 in Vovk and Wang (2021)) ##
#-------------------------------------------------------------------------------
adj = function(e){#vector input
  K = length(e)
  e_sorted = sort(e)
  order_e = order(e)
  S = cumsum(e_sorted)
  e_adj = rep(NA,K)
  for ( i in 1:K){
    e_adj[order_e[i]]= min(((e_sorted[i]+S)/(1+(1:K)))[1:(i-1)])
  }
  return(e_adj) #vector output
}

matrix_adj=function(E){#matrix input
  m = dim(E)[1]
  e = as.vector(E)[-(1+(m+1)*(0:(m-1)))]# erase the diagonal elements
  e_adj =adj(e)
  E_adj= c(0,e_adj[1:m])
  for (i in 2:(m-1)){
    E_adj=c(E_adj,0,e_adj[(1+(i-1)*m):(i*m)])
  }
  E_adj= matrix(c(E_adj,0),nrow = m)
  return(E_adj) #matrix output
}

matrix_merg = function(E){#matrix input
  m = dim(E)[1]
  e = as.vector(E)[-(1+(m+1)*(0:(m-1)))]# erase the diagonal elements
  return(mean(e)) #numeric output
}

#-------------------------------------------------------------------------------
## Adjusting with the Geometric mean (see Section 5 in Vovk and Wang (2021)) ##
#-------------------------------------------------------------------------------

adj_geom = function(e){#vector input
  K = length(e)
  e_sorted = sort(e)
  order_e = order(e)
  S = cumsum(e_sorted)
  e_adj = rep(NA,K)
  for ( i in 1:K){
    e_adj[order_e[i]]= min(((e_sorted[i]+S)/((1+(1:K)))*exp(1)*log(1+(1:K)))[1:(i-1)])
  }
  return(e_adj) #vector output
}
