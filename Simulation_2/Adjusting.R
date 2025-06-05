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
