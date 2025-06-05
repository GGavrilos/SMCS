N = 1000 # number of simulations
n = 1000
m = 9
freq_e = size_e = freq_p = size_p = data.frame(matrix(NA, nrow = N,ncol = n))
colnames(freq_e) = colnames(size_e) = colnames(freq_p) = colnames(size_p)= paste0("t=",1:n)
E_adj = array(NA,dim = c(m, n, N))
p_adj = array(NA,dim = c(m, m, n, N))

for (sim in 1:N){
  if (file.exists(paste0("Simulation=", sim, ".rda")))
  {load(paste0("Simulation=", sim, ".rda"))
    freq_e[sim,] = export$freq_e
    size_e[sim,] = export$size_e
    freq_p[sim,] = export$freq_p
    size_p[sim,] = export$size_p
    # E_adj[, , sim] = export$E_adj
    # p_adj[, , , sim] = 1/export$p_adj_inv
  }
}
freq_e_summarized = colMeans(freq_e, na.rm=TRUE)
size_e_summarized = colMeans(size_e, na.rm=TRUE)
freq_p_summarized = colMeans(freq_p, na.rm=TRUE)
size_p_summarized = colMeans(size_p, na.rm=TRUE)
# E_adj_summarized = apply(E_adj, 1:2, mean, na.rm=TRUE)
# p_adj_summarized = apply(p_adj, 1:3, mean, na.rm=TRUE)



save(freq_e_summarized,file="freq_e_summarized.rda")
save(size_e_summarized,file="size_e_summarized.rda")
save(freq_p_summarized,file="freq_p_summarized.rda")
save(size_p_summarized,file="size_p_summarized.rda")