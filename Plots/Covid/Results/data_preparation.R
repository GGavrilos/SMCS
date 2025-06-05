n = 130
#' CDC_ensemble (1), 4_week_ensemble (2), baseline (3), ensemble (4), GT-deep (5),
#' mobs-gleam (6), psi-draft(7)

# "baseline(1), "MOBS-gleam(2), "PSI-draft(3), "GT-deep(4),"ensemble(5), "CDC-ensemble(6)

#-------------------------------------------------------------------------------
# predictable lambda
# Working directory set in "Covid", outside of the "Plots" directory.

load("Rda files/eadj_01.rda")
df1 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="CDC-ensemble",
                 "MCS"=(1:n)<=eadj_01$value[1],"quantile"=0.1,"method"="smart")
df3 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="baseline",
                 "MCS"=((1:n)<=eadj_01$value[2]),"quantile"=0.1,"method"="smart")
df4 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="ensemble",
                 "MCS"=((1:n)<=eadj_01$value[3]),"quantile"=0.1,"method"="smart")
df5 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="GT-deep",
                 "MCS"=((1:n)<=eadj_01$value[4]),"quantile"=0.1,"method"="smart")
df6 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="MOBS-gleam",
                 "MCS"=((1:n)<=eadj_01$value[5]),"quantile"=0.1,"method"="smart")
df7 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="PSI-draft",
                 "MCS"=((1:n)<=eadj_01$value[6]),"quantile"=0.1,"method"="smart")
data1=rbind(df1, df3, df4, df5, df6, df7)

load("Rda files/eadj_03.rda")
df1 = data.frame("x" = 1:n,"x_end" = c(2:n,NA),"model"="CDC-ensemble",
                 "MCS" = ((1:n) <= eadj_03$value[1]),"quantile"=0.3,"method"="smart")
df3 = data.frame("x" = 1:n,"x_end" = c(2:n,NA),"model"="baseline",
                 "MCS" = ((1:n) <= eadj_03$value[2]),"quantile"=0.3,"method"="smart")
df4 = data.frame("x" = 1:n,"x_end" = c(2:n,NA),"model"="ensemble",
                 "MCS" = ((1:n) <= eadj_03$value[3]),"quantile"=0.3,"method"="smart")
df5 = data.frame("x" = 1:n,"x_end" = c(2:n,NA),"model"="GT-deep",
                 "MCS" = ((1:n) <= eadj_03$value[4]),"quantile"=0.3,"method"="smart")
df6 = data.frame("x" = 1:n,"x_end" = c(2:n,NA),"model"="MOBS-gleam",
                 "MCS" = ((1:n) <= eadj_03$value[5]),"quantile"=0.3,"method"="smart")
df7 = data.frame("x" = 1:n,"x_end" = c(2:n,NA),"model"="PSI-draft",
                 "MCS" = ((1:n) <= eadj_03$value[6]),"quantile"=0.3,"method"="smart")
data2=rbind(df1, df3, df4, df5, df6, df7)

load("Rda files/eadj_05.rda")
df1 = data.frame("x" = 1:n, "x_end" = c(2:n, NA), "model" = "CDC-ensemble",
                 "MCS" = ((1:n) <= eadj_05$value[1]),"quantile" = 0.5, "method" = "smart")
df3 = data.frame("x"=1:n,"x_end" = c(2:n,NA), "model" = "baseline",
                 "MCS" = ((1:n) <= eadj_05$value[2]),"quantile" = 0.5, "method" = "smart")
df4 = data.frame("x" = 1:n,"x_end" = c(2:n,NA), "model" = "ensemble",
                 "MCS" = ((1:n) <= eadj_05$value[3]),"quantile" = 0.5, "method" = "smart")
df5 = data.frame("x" = 1:n,"x_end" = c(2:n,NA), "model" = "GT-deep",
                 "MCS" = ((1:n) <= eadj_05$value[4]),"quantile" = 0.5, "method" = "smart")
df6 = data.frame("x"=1:n,"x_end" = c(2:n,NA), "model" = "MOBS-gleam",
                 "MCS" = ((1:n) <= eadj_05$value[5]),"quantile" = 0.5, "method" = "smart")
df7 = data.frame("x"=1:n,"x_end" = c(2:n,NA), "model" = "PSI-draft",
                 "MCS" = ((1:n) <= eadj_05$value[6]), "quantile" = 0.5,"method" = "smart")
data3=rbind(df1, df3, df4, df5, df6, df7)

load("Rda files/eadj_07.rda")
df1 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="CDC-ensemble",
                 "MCS"=(1:n)<=eadj_07$value[1],"quantile"=0.7,"method"="smart")
df3 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="baseline",
                 "MCS"=((1:n)<=eadj_07$value[2]),"quantile"=0.7,"method"="smart")
df4 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="ensemble",
                 "MCS"=((1:n)<=eadj_07$value[3]),"quantile"=0.7,"method"="smart")
df5 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="GT-deep",
                 "MCS"=((1:n)<=eadj_07$value[4]),"quantile"=0.7,"method"="smart")
df6 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="MOBS-gleam",
                 "MCS"=((1:n)<=eadj_07$value[5]),"quantile"=0.7,"method"="smart")
df7 = data.frame("x"=1:n,"x_end"=c(2:n,NA),"model"="PSI-draft",
                 "MCS"=((1:n)<=eadj_07$value[6]),"quantile"=0.7,"method"="smart")
data4=rbind(df1,df3,df4,df5,df6,df7)

data=rbind(data1, data2, data3, data4)


data$MCS[which((data$model) == "MOBS-gleam")] = 2 * data$MCS[which((data$model) == "MOBS-gleam")]
data$MCS[which((data$model) == "PSI-draft")] = 3 * data$MCS[which((data$model) == "PSI-draft")]
data$MCS[which((data$model) == "GT-deep")] = 4 * data$MCS[which((data$model) == "GT-deep")]
data$MCS[which((data$model) == "ensemble")] = 5 * data$MCS[which((data$model) == "ensemble")]
data$MCS[which((data$model) == "CDC-ensemble")] = 6 * data$MCS[which((data$model) == "CDC-ensemble")]

data$model=as.factor(data$model)
data$model <- factor(data$model, 
  levels=c("baseline", "MOBS-gleam", "PSI-draft", "GT-deep","ensemble", "CDC-ensemble")
                     ,ordered=TRUE)

save(data,file="data_summarized.rda")



