N = 1000 # number of simulations
n = 1000
m = 49

freq_2e = freq_2p = size_2e = size_2p = data.frame(matrix(NA, nrow = N,ncol = n))
colnames(freq_2e) = colnames(freq_2p) = colnames(size_2e) = colnames(size_2p) = paste0("t=",1:n)
E_adj = array(NA,dim = c(m, n, N))
p_adj = array(NA,dim = c(m, m, n, N))

for (sim in 1:N){
  if (file.exists(paste0("Simulation=", sim, ".rda")))
  {load(paste0("Simulation=", sim, ".rda"))
    freq_2e[sim, ] = export$freq_e
    size_2e[sim, ] = export$size_e
    freq_2p[sim, ] = export$freq_p
    size_2p[sim, ] = export$size_p
    # E_adj[, , sim] = export$E_adj
    # p_adj[, , , sim] = 1/export$p_adj_inv
  }
}

freq_2e_summarized = colMeans(freq_2e, na.rm=TRUE)
size_2e_summarized = colMeans(size_2e, na.rm=TRUE)
freq_2p_summarized = colMeans(freq_2p, na.rm=TRUE)
size_2p_summarized = colMeans(size_2p, na.rm=TRUE)

# E_adj_summarized = apply(E_adj, 1:2, mean, na.rm=TRUE)
# p_adj_summarized = apply(p_adj, 1:3, mean, na.rm=TRUE)


save(freq_2e_summarized, file = "freq_2e_summarized.rda")
save(size_2e_summarized, file = "size_2e_summarized.rda")
save(freq_2p_summarized, file = "freq_2p_summarized.rda")
save(size_2p_summarized, file = "size_2p_summarized.rda")

