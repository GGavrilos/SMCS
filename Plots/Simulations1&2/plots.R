#### Plots for Simulation 1&2 ####

library(ggplot2)
library(ggplot2bdc)
#library(dplyr)
n = 1000
#Simulation 1, e-processes 
load("sim1_size_e_summarized.rda")
size_e_summarized = size_e_summarized[1:n] #plot only 1000 first data points
data1 <- data.frame(
  index = 1:1000,
  value = size_e_summarized,
  sim = "Simulation 1",
  style = "e"
)

# Simulation 2, e-processes 
load("sim2_size_2e_summarized.rda") 
size_2e_summarized=size_2e_summarized[1:n]#plot only 1000 first data points
data2 <- data.frame(
  index = 1:1000,
  value = size_2e_summarized,
  sim = "Simulation 2",
  style = "e"
)


# Simulation 1, Hansen 
load("mcs_hansen.rda")
data5 <- data.frame(
  index = 1:1000,
  value = colMeans(mcs_hansen),
  sim = "Simulation 1 (MCS)",
  style = "MCS"
)

# combined_plot
data= rbind(data1, data2)
data$sim = as.factor(data$sim)
data$sim = factor(data$sim, levels=c("Simulation 1", "Simulation 2"),ordered=TRUE)


p <- ggplot(data = data) + 
  geom_line(aes(x = index, y = value, 
                colour = sim, 
                linetype = style, 
                linewidth = style)) +  # linewidth maps to style
  xlab(substitute(paste(italic('t')))) +
  ggtitle("Average size of the SMCS") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = c("Simulation 1" = "#1b9e77", "Simulation 2" = "#d95f02")) +
  scale_linetype_manual(values = c("e" = "solid"), guide = "none") +
  scale_linewidth_manual(values = c("e" = 0.65), guide = "none")
p


ggsave_golden(filename = "Simulations1&2.pdf",plot=p)



##### additional: the coverage over time #####

#Simulation 1, e-processes 
load("sim1_freq_e_summarized.rda")
freq_e_summarized = freq_e_summarized[1:n] #plot only 1000 first data points
data1 <- data.frame(
  index = 1:1000,
  value = freq_e_summarized,
  sim = "Simulation 1 (SMCS)",
  style = "e"
)

# Simulation 2, e-processes 
load("sim2_freq_2e_summarized.rda") 
freq_2e_summarized=freq_2e_summarized[1:n]#plot only 1000 first data points
data2 <- data.frame(
  index = 1:1000,
  value = freq_2e_summarized,
  sim = "Simulation 2 (SMCS)",
  style = "e"
)

# Simulation 1, Hansen 
load("coverage.rda")
data5 <- data.frame(
  index = 1:1000,
  value = colMeans(coverage),
  sim = "Simulation 1 (MCS)",
  style = "MCS"
)

# combined_plot
data= rbind(data1, data2)
data$sim = as.factor(data$sim)
data$sim = factor(data$sim, levels=c("Simulation 1 (SMCS)","Simulation 2 (SMCS)"),ordered=TRUE)


p <- ggplot(data = data) + 
  geom_line(aes(x = index, y = value, 
                colour = sim, 
                linetype = style, 
                linewidth = style)) +  # linewidth maps to style
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black", linewidth = 0.2) +  
  xlab(substitute(paste(italic('t')))) +
  ggtitle("Coverage of the superior model") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size = 26),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 13)) +
  scale_color_manual(values = c("Simulation 1 (SMCS)" = "#1b9e77", "Simulation 2 (SMCS)" = "#d95f02")) +
  scale_linetype_manual(values = c("e" = "solid"), guide = "none") +
  scale_linewidth_manual(values = c("e" = 0.65), guide = "none")
p



ggsave_golden(filename = "Simulations1&2_freq.pdf",plot=p)

