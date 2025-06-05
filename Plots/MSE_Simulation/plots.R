###############################################################################
############################ size plots #######################################
###############################################################################

#------------------------- load libraries  -------------------------------------
library(ggplot2)
library(ggplot2bdc)
library(RColorBrewer)

#----------------------- define the colors -------------------------------------
m = 9
mycol = brewer.pal(m, "Set1")

#----------------------- prepare data-frames -----------------------------------
n = 1000
load("size_e_summarized.rda")
data1 <- data.frame(
  index = 1:n,
  value = size_e_summarized,
  style = "e-based"
)


# Simulation 2, e-processes 
# load("size_p_summarized.rda")
# data2 <- data.frame(
#  index = 1:n,
#  value = size_p_summarized,
#  style = "p-based"
#)

# data= rbind(data1,data2)
# data$style = as.factor(data$style)
# data$style = factor(data$style, levels=c("e-based","p-based"),ordered=TRUE)


p <- ggplot(data = data1) + 
  geom_line(aes(x = index, y = value, 
                colour = style, 
                linetype = style, 
                linewidth = style)) + 
  xlab(substitute(paste(italic('t')))) +
  ggtitle("Average size of the SMCS") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20)) +
  scale_color_manual(values = c("e-based" = "#1b9e77")) +
  scale_linetype_manual(values = c("e-based" = 1), guide = "none") +
  scale_linewidth_manual(values = c("e-based" = 1.2), guide = "none")
p

ggsave_golden(filename = "size_plot.pdf",plot=p)

###############################################################################
###################### plots the e-processes ##################################
###############################################################################

load("E_adj_summarized.rda")
n = 1000

data = data.frame(
  index = 1:n,
  value = log(E_adj_summarized[1,]),
  model = 1
)

for (i in 2:m){
df=  data.frame(
    index = 1:n,
    value = log(E_adj_summarized[i,]),
    model = i)
data= rbind(data,df)
}


data$model = as.factor(data$model)
data$model = factor(data$model,#, levels=c("e-based","p-based"),
                    ordered=TRUE)


p <- ggplot(data = data) + 
  geom_line(aes(x = index, y = value, 
                colour = model), linewidth = 1.15) + 
  xlab(substitute(paste(italic('t')))) +
  geom_hline(yintercept = log(1/0.1), color = "black", linetype = "dashed", linewidth = 0.1)+
  ggtitle("e-processes") +
  theme_bw() +
  guides(color = guide_legend(nrow = 1)) +          
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 19),
        plot.title = element_text(size = 20)) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1.3), nrow = 1)  # Adjust legend line width
  ) + 
  scale_color_manual(values = mycol) +
  coord_cartesian(ylim = c(-15, 20))

p

ggsave_golden(filename = "e-processes.pdf",plot=p)

###############################################################################
###################### plots the p-processes ##################################
###############################################################################

# load("p_adj_summarized.rda")
# mycol = mycol[-2]
#n = 1000
#data = data.frame(
#  index=1:n,
#  value = p_adj_summarized[1,2,],
#  model = 1
#)
# df =  data.frame(
#  index = 1:n,
#  value = p_adj_summarized[2,5,],
#  model = 2)
#data= rbind(data, df)

#for (i in 3:m){
#  df=  data.frame(
#    index = 1:n,
#    value = p_adj_summarized[i,2,],
#    model = i)
#  data= rbind(data,df)
#}


# data$model = as.factor(data$model)
# data$model = factor(data$model,ordered=TRUE)


# p <- ggplot(data = data) + 
#  geom_line(aes(x = index, y = value, 
#                colour = model)) + 
#  xlab(substitute(paste(italic('t')))) +
#  geom_hline(yintercept = 0.1, color = "black", linetype = "dashed", size = 0.1)+
#  ggtitle("p-processes") +
#  theme_bw() +
#  guides(color = guide_legend(nrow = 1)) +          
#  theme(axis.title.y = element_blank(),
#        legend.title = element_blank(),
#        legend.position = "bottom",
#        legend.text = element_text(size = 12)) +
#  scale_color_manual(values = mycol)+
#  coord_cartesian(ylim = c(0, 0.75))
# p
# ggsave_golden(path = "~/Dropbox/GitHub/MSE_Simulation",
#               filename = "p-processes.pdf",plot=p)

