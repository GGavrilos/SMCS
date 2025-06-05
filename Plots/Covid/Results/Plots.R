library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(rlang)
library(grid)
library(gridExtra) 
library(ggpubr)
library(ggplot2bdc)


m = 6
mypalette = brewer.pal(m, "Set1")

library(lubridate)
load("truth.rda")
times= as.POSIXct(truth$target_end_date, format = "%Y-%m-%d", tz = "UTC")
years = year(times)
unique_years=ind=unique(years)[-1]
for (j in 1:length(unique_years)){ind[j] = min(which(years==unique_years[j]))}

###############################################################################
########################### plots for results #################################
###############################################################################

load("data_summarized.rda")

p1= ggplot(data = subset(data,quantile==0.1), aes(x = x, y = MCS, xend = x_end, yend = MCS, color = model)) +
  geom_segment(linewidth = 10, alpha = 1)+
  coord_cartesian(ylim = c(0.9, 6.1)) + 
  labs(title=expression(tau~"=0.1"))+
  scale_y_continuous(breaks=1:6,labels=rep("",6))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.x = element_text(size = 24),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 26))+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)

p2= ggplot(subset(data,quantile==0.3), aes(x=x, y=MCS, xend=x_end, yend=MCS,color=model)) +
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,6.1)) + 
  labs(title=expression(tau~"=0.3"))+
  scale_y_continuous(breaks=1:6,labels=rep("",6))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.x = element_text(size = 24),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 26))+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)


p3= ggplot(subset(data,quantile==0.5), aes(x=x, y=MCS, xend=x_end, yend=MCS,color=model)) +
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,6.1)) + 
  labs(title=expression(tau~"=0.5"))+
  scale_y_continuous(breaks=1:6,labels=rep("",6))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.x = element_text(size = 24),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 26))+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)


p4= ggplot(subset(data,quantile==0.7), aes(x=x, y=MCS, xend=x_end, yend=MCS,color=model)) +
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,6.1)) + 
  labs(title=expression(tau~"=0.7"))+
  scale_y_continuous(breaks=1:6,labels=rep("",6))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.x = element_text(size = 24),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 26))+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)


# call last plot to extract legend
p5= ggplot(subset(data,quantile==0.5), aes(x=x, y=MCS, xend=x_end, yend=MCS,color=model)) +
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,6.1)) + 
  labs(title=expression(tau~"=0.9"))+
  scale_y_continuous(breaks=1:6,labels=rep("",6))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 26))+ #remove y axis labels
  scale_color_manual(values=mypalette)
p5

legend_b = get_legend(p5+
                        guides(color=guide_legend(nrow=1))+
                        theme(legend.title=element_blank(),
                              legend.position = "bottom",
                              legend.text = element_text(size = 13)))

legend = as_ggplot(legend_b)
legend+theme(legend.box.margin=margin(c(0,0,0,0)))
ggsave("Covid_legend.pdf", width = 8, height = 0.8)

ggsave_golden(filename = "covid_results_01.pdf",plot=p1)
ggsave_golden(filename = "covid_results_03.pdf",plot=p2)
ggsave_golden(filename = "covid_results_05.pdf",plot=p3)
ggsave_golden(filename = "covid_results_07.pdf",plot=p4)
