matrix_to_vector=function(X){#mxm matrix with NA/0 diagonal entries as an input
  m = dim(X)[1]
  x = as.numeric(t(X))[-(1+(m+1)*(0:(m-1)))]# erase the diagonal elements
  return(x) #vector of length m(m-1)
}


vector_to_matrix=function(x){#vector of length k=m(m-1) as an input
 k = length(x)
 m=(1+sqrt(1+4*k))/2
 if (m>2){
   X= c(0,x[1:m])
   for (i in 2:(m-1)){
      X=c(X,0,x[(1+(i-1)*m):(i*m)])
   }
   X= matrix(c(X,0),nrow = m,byrow = TRUE)}
  if (m==2){
    X= matrix(c(0,x,0),nrow = m,byrow = TRUE)
  }
  return(X) #mxm matrix with diagonal entries set to zero
}

matrix_mean=function(X){#mxm matrix with NA/0 diagonal entries as an input
  return(mean(matrix_to_vector(X)))
}

idr_func = function(x,idr_cdf){
  y = x
  for (i in 1:length(y)){
    if (x[i]<idr_cdf[1,1]){y[i]=0}
    else{ 
      tmp=max(which(x[i]>=idr_cdf[,1]))
      y[i]=idr_cdf[tmp,2]
    }
  }
  return(y)
}
