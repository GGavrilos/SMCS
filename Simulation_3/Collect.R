n= 800

freq = data.frame(matrix(NA,nrow=1,ncol=n))
size = data.frame(matrix(NA,nrow=1,ncol=n))
colnames(freq)=colnames(size)= paste0("t=",1:n)


 for (sim in 1:100){
    if (file.exists(paste0("Simulation=",sim,".rda")))
      {load(paste0("Simulation=",sim,".rda"))
          freq= rbind(freq,export$freq)
          size = rbind(size, export$size)
      }
    }
  

freq = freq[-1,]
size = size[-1,]
save(freq,file="freq.rda")
save(size,file="size.rda")
freq_summarized=colMeans(freq)
size_summarized = colMeans(size)
save(freq_summarized,file="freq_summarized.rda")
save(size_summarized,file="size_summarized.rda")
