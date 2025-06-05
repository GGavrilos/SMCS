############################## #############################
################# Simulation 3 #############################
############################## #############################

library(ggplot2)
library(ggplot2bdc)
library(RColorBrewer)
mypalette<-brewer.pal(8,"Set1")[c(1,2,3)]


library(grid)
library(gridExtra) 
library(ggpubr)
library(rlang)
library(cowplot)


n = 800
t=1:n
t1=154
t2=550
load("~/Desktop/EMCS/Plots_paper/Simulation3/MCS_one_simulation.rda")

df1 = data.frame("t"=t)
df1[,2]=1
df1[,3]=cumsum(export$L[,1])
df1[,4]= export$MCS[1,]
df2 = data.frame("t"=t)
df2[,2]=2
df2[,3]=cumsum(export$L[,2])
df2[,4]= export$MCS[2,]
df3 = data.frame("t"=t)
df3[,2]=3
df3[,3]=cumsum(export$L[,3])
df3[,4]= export$MCS[3,]
data1=rbind(df1,df2, df3)
colnames(data1)= c("t","Model","loss","MCS")
data1$MCS[(data1$Model==1) & (data1$MCS==1)] = 75
data1$MCS[(data1$Model==2) & (data1$MCS==1)] = 87.5
data1$MCS[(data1$Model==3) & (data1$MCS==1)] = 100

data1$MCS[(data1$MCS==0)] =NA
tt = c(t[2:n],NA)
data1$tend=rep(tt,3)
data1$MCSend=data1$MCS
data1$Model=as.factor(data1$Model)
data1$Model <- factor(data1$Model, levels=c("1","2","3"),ordered=TRUE)



### Plot
p2= ggplot(data1, aes(x=t, y=MCS, xend=tend, yend=MCSend,color=Model)) +
  geom_vline(xintercept = c(t1,t2), size=0.5,alpha=0.5)+
  geom_segment(linewidth=7)+
  geom_line(data=data1,aes(t,loss,color=Model),linewidth=1)+
  coord_cartesian(ylim=c(0,100)) + 
  theme_bw()+
  #scale_y_continuous(breaks=c(1,2,3),labels=c("1", "2", "3"))+
  theme(axis.title.y=element_blank(),
        legend.position = "right")+ #remove y axis labels
  xlab(substitute(paste(italic('t'))))+
  ggtitle("One realization of simulation 3")+
  scale_color_manual(values=mypalette)
p2

ggsave_golden(path = "~/Desktop/EMCS/Plots_paper/Simulation3",
              filename = "Sim3.pdf",plot=p2)

