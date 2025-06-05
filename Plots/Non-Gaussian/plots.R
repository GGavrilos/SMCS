#### Plots for probabilistic simulation ####


setwd("~/polybox - Georgios Gavrilopoulos (georgios.gavrilopoulos@stat.math.ethz.ch)@polybox.ethz.ch/PhD/Projects/SMCS/RCode/Simulation_1/")

load("Non-Gaussian/MCS/coverage.rda")
load("Non-Gaussian/MCS/mcs_hansen.rda")

load("Non-Gaussian/SMCS_new/freq_e.rda")
load("Non-Gaussian/SMCS_new/size_e.rda")

load("Non-Gaussian/SMCS_new/freq_p.rda")
load("Non-Gaussian/SMCS_new/size_p.rda")


library(ggplot2)
library(patchwork)
library(dplyr)

mcs_dataframe <- data.frame(
  index = 1:1000,
  coverage = colMeans(coverage)[1:1000],
  size_hansen = colMeans(mcs_hansen)[1:1000]
)

evalue_dataframe <- data.frame(
  index = 1:1000,
  coverage = colMeans(freq_e)[1:1000],
  size_smcs = colMeans(size_e)[1:1000]
)

N = 1000 #Number of Monte Carlo iterations

p <- ggplot() +
  geom_line(mcs_dataframe, mapping = aes(x = index, y = coverage, colour = "MCS"), linewidth = 1) +
  geom_line(evalue_dataframe, mapping = aes(x = index, y = coverage, colour = "SMCS"), linewidth = 1) +
  scale_colour_manual(values = c("MCS" = "red", "SMCS" = "blue")) +
  xlab("Time") +
  ylab("Average Coverage Rates") +
  ylim(0.2, 1) +
  ggtitle(paste0("Coverage Rates (N = ", N, ")")) +
  theme_grey(base_size = 18) +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "gray90"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "gray90"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7)) +
  labs(colour="Method")

q <- ggplot() +
  geom_line(mcs_dataframe, mapping = aes(x = index, y = size_hansen, colour = "MCS"), linewidth = 1) +
  geom_line(evalue_dataframe, mapping = aes(x = index, y = size_smcs, colour = "SMCS"), linewidth = 1) +
  scale_colour_manual(values = c("MCS" = "red", "SMCS" = "blue")) +
  xlab("Time") +
  ylim(0, 50) +
  ylab("Average Size") +
  ggtitle(paste0("Size of Conf. Set (N = ", N, ")")) +
  theme_grey(base_size = 18) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey90"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "gray90"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7)) +
  labs(colour="Method")

p + q + plot_layout(guides = "collect") + theme(legend.position = "right")
