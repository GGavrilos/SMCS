m = 9

library(ggplot2)
library(ggplot2bdc)
library(RColorBrewer)
library(lubridate)
mypalette=brewer.pal(m,"Set1")

# Plot1: Station 10168
load("station=10168.rda")
for (i in 1:m){export$elements[,i]=i*export$elements[,i]}
MCS=export$elements
n=nrow(MCS)
df1=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=1,"MCS"=MCS[,1])
df2=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=2,"MCS"=MCS[,2])
df3=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=3,"MCS"=MCS[,3])
df4=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=4,"MCS"=MCS[,4])
df5=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=5,"MCS"=MCS[,5])
df6=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=6,"MCS"=MCS[,6])
df7=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=7,"MCS"=MCS[,7])
df8=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=8,"MCS"=MCS[,8])
df9=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=9,"MCS"=MCS[,9])
data=rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)
data$model=as.factor(data$model)
data$model <- factor(data$model, levels=paste0(1:10),ordered=TRUE)

times= as.POSIXct(rownames(MCS), format = "%Y-%m-%d", tz = "UTC")
years = year(times)
unique_years=ind=unique(years)
for (j in 1:length(unique_years)){ind[j] = min(which(years==unique_years[j]))}
tmp=decimal_date(ymd(c("2017-03-22","2018-05-16","2021-02-10")))
for (j in 1:3){tmp[j]= min(which(decimal_date(times)>=tmp[j]))}

p1= ggplot(data, aes(x=t, y=MCS, xend=tend, yend=MCS,color=model)) +
  geom_vline(xintercept = tmp, linewidth=0.5,alpha=0.5)+
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,9.1)) + 
  ggtitle("Station 10168")+
  scale_y_continuous(breaks=1:9,labels=c("ens","emos","mbm","idr","gb","qrf","drn","bqn","hen"))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24))+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)
#p1

# Plot2: Station 10376
load("station=10376.rda")
for (i in 1:m){export$elements[,i]=i*export$elements[,i]}
MCS=export$elements
n=nrow(MCS)
df1=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=1,"MCS"=MCS[,1])
df2=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=2,"MCS"=MCS[,2])
df3=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=3,"MCS"=MCS[,3])
df4=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=4,"MCS"=MCS[,4])
df5=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=5,"MCS"=MCS[,5])
df6=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=6,"MCS"=MCS[,6])
df7=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=7,"MCS"=MCS[,7])
df8=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=8,"MCS"=MCS[,8])
df9=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=9,"MCS"=MCS[,9])
data=rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)
data$model=as.factor(data$model)
data$model <- factor(data$model, levels=paste0(1:10),ordered=TRUE)

times= as.POSIXct(rownames(MCS), format = "%Y-%m-%d", tz = "UTC")
years = year(times)
unique_years=ind=unique(years)
for (j in 1:length(unique_years)){ind[j] = min(which(years==unique_years[j]))}
tmp=decimal_date(ymd(c("2017-03-22","2018-05-16","2021-02-10")))
for (j in 1:3){tmp[j]= min(which(decimal_date(times)>=tmp[j]))}

p2= ggplot(data, aes(x=t, y=MCS, xend=tend, yend=MCS,color=model)) +
  geom_vline(xintercept = tmp, linewidth=0.5,alpha=0.5)+
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,9.1)) + 
  ggtitle("Station 10376")+
  scale_y_continuous(breaks=1:9,labels=c("ens","emos","mbm","idr","gb","qrf","drn","bqn","hen"))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24),)+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)

# Plot3: Station 10557
load("station=10557.rda")
for (i in 1:m){export$elements[,i]=i*export$elements[,i]}
MCS=export$elements
n=nrow(MCS)
df1=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=1,"MCS"=MCS[,1])
df2=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=2,"MCS"=MCS[,2])
df3=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=3,"MCS"=MCS[,3])
df4=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=4,"MCS"=MCS[,4])
df5=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=5,"MCS"=MCS[,5])
df6=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=6,"MCS"=MCS[,6])
df7=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=7,"MCS"=MCS[,7])
df8=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=8,"MCS"=MCS[,8])
df9=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=9,"MCS"=MCS[,9])
data=rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)
data$model=as.factor(data$model)
data$model <- factor(data$model, levels=paste0(1:10),ordered=TRUE)

times= as.POSIXct(rownames(MCS), format = "%Y-%m-%d", tz = "UTC")
years = year(times)
unique_years=ind=unique(years)
for (j in 1:length(unique_years)){ind[j] = min(which(years==unique_years[j]))}
tmp=decimal_date(ymd(c("2017-03-22","2018-05-16","2021-02-10")))
for (j in 1:3){tmp[j]= min(which(decimal_date(times)>=tmp[j]))}

p3= ggplot(data, aes(x=t, y=MCS, xend=tend, yend=MCS,color=model)) +
  geom_vline(xintercept = tmp, linewidth=0.5,alpha=0.5)+
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,9.1)) + 
  ggtitle("Station 10557")+
  scale_y_continuous(breaks=1:9,labels=c("ens","emos","mbm","idr","gb","qrf","drn","bqn","hen"))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24))+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)

# Plot4: Station 10648
load("station=10648.rda")

for (i in 1:m){export$elements[,i]=i*export$elements[,i]}
MCS=export$elements
n=nrow(MCS)
df1=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=1,"MCS"=MCS[,1])
df2=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=2,"MCS"=MCS[,2])
df3=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=3,"MCS"=MCS[,3])
df4=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=4,"MCS"=MCS[,4])
df5=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=5,"MCS"=MCS[,5])
df6=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=6,"MCS"=MCS[,6])
df7=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=7,"MCS"=MCS[,7])
df8=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=8,"MCS"=MCS[,8])
df9=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=9,"MCS"=MCS[,9])
data=rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)
data$model=as.factor(data$model)
data$model <- factor(data$model, levels=paste0(1:10),ordered=TRUE)

times= as.POSIXct(rownames(MCS), format = "%Y-%m-%d", tz = "UTC")
years = year(times)
unique_years=ind=unique(years)
for (j in 1:length(unique_years)){ind[j] = min(which(years==unique_years[j]))}
tmp=decimal_date(ymd(c("2017-03-22","2018-05-16","2021-02-10")))
for (j in 1:3){tmp[j]= min(which(decimal_date(times)>=tmp[j]))}

p4= ggplot(data, aes(x=t, y=MCS, xend=tend, yend=MCS,color=model)) +
  geom_vline(xintercept = tmp, linewidth=0.5,alpha=0.5)+
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,9.1)) + 
  ggtitle("Station 10648")+
  scale_y_continuous(breaks=1:9,labels=c("ens","emos","mbm","idr","gb","qrf","drn","bqn","hen"))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24))+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)

# Plot5: Station 10193
load("station=10193.rda")

for (i in 1:m){export$elements[,i]=i*export$elements[,i]}
MCS=export$elements
n=nrow(MCS)
df1=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=1,"MCS"=MCS[,1])
df2=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=2,"MCS"=MCS[,2])
df3=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=3,"MCS"=MCS[,3])
df4=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=4,"MCS"=MCS[,4])
df5=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=5,"MCS"=MCS[,5])
df6=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=6,"MCS"=MCS[,6])
df7=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=7,"MCS"=MCS[,7])
df8=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=8,"MCS"=MCS[,8])
df9=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=9,"MCS"=MCS[,9])
data=rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)
data$model=as.factor(data$model)
data$model <- factor(data$model, levels=paste0(1:10),ordered=TRUE)

times= as.POSIXct(rownames(MCS), format = "%Y-%m-%d", tz = "UTC")
years = year(times)
unique_years=ind=unique(years)
for (j in 1:length(unique_years)){ind[j] = min(which(years==unique_years[j]))}
tmp=decimal_date(ymd(c("2017-03-22","2018-05-16","2021-02-10")))
for (j in 1:3){tmp[j]= min(which(decimal_date(times)>=tmp[j]))}

p5= ggplot(data, aes(x=t, y=MCS, xend=tend, yend=MCS,color=model)) +
  geom_vline(xintercept = tmp, linewidth=0.5,alpha=0.5)+
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,9.1)) + 
  ggtitle("Station 10193")+
  scale_y_continuous(breaks=1:9,labels=c("ens","emos","mbm","idr","gb","qrf","drn","bqn","hen"))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24))+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)
p5

# Plot6: Station 10385
load("station=10385.rda")

for (i in 1:m){export$elements[,i]=i*export$elements[,i]}
MCS=export$elements
n=nrow(MCS)
df1=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=1,"MCS"=MCS[,1])
df2=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=2,"MCS"=MCS[,2])
df3=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=3,"MCS"=MCS[,3])
df4=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=4,"MCS"=MCS[,4])
df5=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=5,"MCS"=MCS[,5])
df6=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=6,"MCS"=MCS[,6])
df7=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=7,"MCS"=MCS[,7])
df8=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=8,"MCS"=MCS[,8])
df9=data.frame("t"=1:n,"tend"=c(2:n,NA),"model"=9,"MCS"=MCS[,9])
data=rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)
data$model=as.factor(data$model)
data$model <- factor(data$model, levels=paste0(1:10),ordered=TRUE)

times= as.POSIXct(rownames(MCS), format = "%Y-%m-%d", tz = "UTC")
years = year(times)
unique_years=ind=unique(years)
for (j in 1:length(unique_years)){ind[j] = min(which(years==unique_years[j]))}
tmp=decimal_date(ymd(c("2017-03-22","2018-05-16","2021-02-10")))
for (j in 1:3){tmp[j]= min(which(decimal_date(times)>=tmp[j]))}

p6= ggplot(data, aes(x=t, y=MCS, xend=tend, yend=MCS,color=model)) +
  geom_vline(xintercept = tmp, linewidth=0.5,alpha=0.5)+
  geom_segment(linewidth=10,alpha=1)+
  coord_cartesian(ylim=c(0.9,9.1)) + 
  ggtitle("Station 10385")+
  scale_y_continuous(breaks=1:9,labels=c("ens","emos","mbm","idr","gb","qrf","drn","bqn","hen"))+
  scale_x_continuous(breaks=ind,labels=unique_years)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 24))+ #remove y axis labels
  theme(legend.position = "none")+
  scale_color_manual(values=mypalette)
p6

#export 
ggsave_golden(filename = "Station10168_white.pdf",plot=p1)
ggsave_golden(filename = "Station10376_white.pdf",plot=p2)
ggsave_golden(filename = "Station10557_white.pdf",plot=p3)
ggsave_golden(filename = "Station10648_white.pdf",plot=p4)
ggsave_golden(filename = "Station10193_white.pdf",plot=p5)
ggsave_golden(filename = "Station10385_white.pdf",plot=p6)

