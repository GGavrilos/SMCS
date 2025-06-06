library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(rlang)
library(grid)
library(gridExtra) 
library(ggpubr)
library(ggplot2bdc)

m=6
mypalette=brewer.pal(m,"Set1")

library(lubridate)
load("truth.rda")
times= as.POSIXct(truth$target_end_date, format = "%Y-%m-%d", tz = "UTC")
years = year(times)
unique_years=ind=unique(years)[-1]
for (j in 1:length(unique_years)){ind[j] = min(which(years==unique_years[j]))}

###############################################################################
############################## time series ####################################
###############################################################################

load("data_median_predictions.rda")

p1=ggplot(data=data) +
  geom_line(aes(x = x, y = value, colour = model),linewidth=0.3)+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  labs(y="linear",title="Covid deaths and median forecasts")+
  theme(axis.title.x=element_blank(),
        legend.title=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24),
        axis.title.y = element_text(size = 22),
        legend.position='none') +
  scale_color_brewer(palette="Dark2")
p1

p2=ggplot(data=data) +
  geom_line(aes(x = x, y = log(value), colour = model),linewidth=0.3)+
  labs(y="log", title="Covid deaths and median forecasts")+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        legend.title=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24),
        axis.title.y = element_text(size = 22),
        legend.position='none') +
  scale_color_brewer(palette="Dark2")
p2


# call third plot for exrtracting the legend

p3=ggplot(data=data) +
  geom_line(aes(x = x, y = value, colour = model))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  labs(y="linear",title="Covid deaths and median forecasts")+
  theme(axis.title.x=element_blank(),
        legend.title=element_blank(), 
        legend.position='bottom') +#remove y axis labels+
  scale_color_brewer(palette="Dark2")
legend_b = get_legend(p3+
                        guides(color=guide_legend(nrow=1))+
                        theme(legend.title=element_blank(),
                              legend.position = "bottom"))

legend = as_ggplot(legend_b)
legend+theme(legend.box.margin=margin(c(0,0,0,0)))
ggsave("Covid_deaths_legend.pdf", width = 4, height = 0.5)


load("tail_quantile_predictions.rda")


p3=ggplot(data=subset(data,quantile==0.15)) +
  geom_line(aes(x = x, y = log(value), colour = model),linewidth=0.3)+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  labs(y="log",title=expression("Quantile forecasts for "~tau~"=0.15"))+
  theme(axis.title.x=element_blank(),
        legend.title=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24),
        axis.title.y = element_text(size = 22),
        legend.position='none') +
  scale_color_brewer(palette="Dark2")
p3

p4=ggplot(data=subset(data,quantile==0.975)) +
  geom_line(aes(x = x, y = log(value), colour = model),linewidth=0.3)+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  labs(y="log",title=expression("Quantile forecasts for "~tau~"=0.975"))+
  theme(axis.title.x=element_blank(),
        legend.title=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24),
        axis.title.y = element_text(size = 22),
        legend.position='none') +
  scale_color_brewer(palette="Dark2")
p4


ggsave_golden(filename = "time_series1.pdf",plot=p1)
ggsave_golden(filename = "time_series2.pdf",plot=p2)
ggsave_golden(filename = "time_series3.pdf",plot=p3)
ggsave_golden(filename = "time_series4.pdf",plot=p4)
