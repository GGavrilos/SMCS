iteration = seq(1, 1000, by = 1)
N = length(iteration)

coverage <- data.frame(matrix(NA, nrow=1, ncol=N))
mcs_hansen <- data.frame(matrix(NA, nrow=1, ncol=N))

colnames(coverage) = colnames(mcs_hansen) = paste0("t=", iteration)

for (sim in 1:1000){
  if (file.exists(paste0("MCS=", sim, ".rda"))) {
    load(paste0("MCS=", sim, ".rda"))
    coverage = rbind(coverage, export$Frequency)
    mcs_hansen = rbind(mcs_hansen, export$Size_MCS)
  }
}

mcs_hansen <- mcs_hansen[-1,]
coverage <- coverage[-1,]

save(coverage, file="coverage.rda")
save(mcs_hansen, file="mcs_hansen.rda")