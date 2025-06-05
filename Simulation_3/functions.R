matrix_to_vector=function(X){#mxm matrix with NA/0 diagonal entries as an input
  m = dim(X)[1]
  x = as.numeric(t(X))[-(1+(m+1)*(0:(m-1)))]# erase the diagonal elements
 # Names=matrix(NA,ncol=m,nrow=m)
 # for(i in 1:m){
 #   for (j in 1:m){
 #     Names[i,j]=paste0(i,",",j)
 #   }
 # }
 # Names = as.character(t(Names))[-(1+(m+1)*(0:(m-1)))]# erase the diagonal elements
 # names(x)=Names
  return(x) #vector of length m(m-1)
}


vector_to_matrix=function(x){#input is a vector of length k=mx(m-1) 
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
  return(X) #vector of length m(m-1)
}

matrix_mean=function(X){#mxm matrix with NA/0 diagonal entries as an input
  return(mean(matrix_to_vector(X)))
}