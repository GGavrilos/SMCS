#### Plot for Simulation 3 ####

library(ggplot2)
library(ggplot2bdc)
#library(dplyr)

#Plots for Simulation 1
load("~/Desktop/EMCS/Plots_paper/Simulation3/Results_27.2.24/size_summarized.rda")
data1 <- data.frame(
  index = 1:800,
  value = size_summarized
)

t1=154
t2=550
mypalette = brewer.pal(8,"Dark2")[2]

p=ggplot(data=data1) +
  geom_vline(xintercept = c(t1,t2), linewidth=0.5,alpha=0.5)+
  geom_line(mapping = aes(x = index, y = value),color=mypalette, linewidth = 1)+
  xlab(substitute(paste(italic('t'))))+
  ggtitle("Average size of the SMCS")+
  theme_bw()+
  theme(axis.title.y=element_blank(), #remove y axis label
      legend.position = "none")
p

ggsave_golden(path = "~/Desktop/EMCS/Plots_paper/Simulation3",
              filename = "Simulation3_results.pdf",plot=p)
