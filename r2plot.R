rm(list=ls())
library(tidyverse)
library(lme4)
library(MuMIn)

ofc<-read.csv('/Users/christinamaher/Desktop/fellowship_data/lpfc_time_master.csv', header = TRUE) 

unique(ofc$X5)[27]

elec <- c()
r2 <- c()
timepoint <- c()
time <- c()
pvalue <- c()

for (e in 1:length(unique(ofc$X5))) {
  elec_data <- ofc[ofc$X5 == unique(ofc$X5)[e], ]
  for (t in 1:length(unique(elec_data$X1))){
    time_data <- elec_data[elec_data$X1 == unique(elec_data$X1)[t], ]
    model <- lm(X4 ~ X11, data = time_data)
    model_details <- summary(model)
    p_temp <- model_details$coefficients[8]
    r2_temp <- model_details$r.squared
    r2 <- c(r2,r2_temp)
    elec <- c(elec,e)
    timepoint <- c(timepoint,t)
    time <- c(time,unique(elec_data$X1)[t])
    pvalue <- c(pvalue,p_temp)
  }
}

ofc_r2 <- data.frame(time,elec,r2,timepoint,pvalue)
selected_rows <- ofc_r2[seq(1, nrow(good_elec_ofc), by = 50), , drop = FALSE]
plot(ofc_r2$r2)

ofc_r2$time <- round(ofc_r2$time,digits=2)
ofc_r2 <- ofc_r2[ofc_r2$elec == 1, ]
x <- ofc_r2 %>% group_by(time) %>% summarise(avgr2 = mean(r2),se=sd(r2)/n())


selected_rows <- ofc_r2[seq(1, nrow(ofc_r2), by = 50), , drop = FALSE]
selected_rows$time <- round(selected_rows$time,digits=2)
x <- selected_rows %>% group_by(time) %>% summarise(avgr2 = mean(r2),se=sd(r2)/n())
plot(x$avgr2,type='l')

rolling_median <- rollapply(ofc_r2_sorted$r2, width = 200, FUN = mean, by.column = FALSE, align = "right")

plot(rolling_median, type = "l")
plot(x$avgr2,type = "l")
length(ofc_r2$r2)
length(ofc_r2$weights)




ggplot(ofc_r2, aes(x = time, y = r2, color = as.factor(elec), group = as.factor(elec))) +
  geom_line() +
  facet_wrap(~ as.factor(elec)) +
  labs(x = "Time", y = "Value") +
  scale_color_discrete(name = "Electrode")  # Customize the legend title



good_elec_ofc <- ofc_r2[ofc_r2$elec == 11, ]
rolling_mean <- rollapply(good_elec_ofc$r2, width = 50, FUN = mean, by.column = FALSE, align = "right")
plot(rolling_mean)

elec11 <- data.frame(rolling_mean)
elec11$time <- c(1:length(rolling_mean))
elec11$elec <- c('11')

good_lpfc_df <- elec11

good_ofc_df %>%
  ggplot( aes(x=time, y=rolling_mean, group=elec, color=elec)) +
  geom_line()+
  geom_vline(xintercept = 750, col = "black",linetype = "dashed") + theme_classic()


good_lpfc_df %>%
  ggplot( aes(x=time, y=rolling_mean, color=elec)) +
  geom_line()+
  geom_vline(xintercept = 750, col = "black",linetype = "dashed") + theme_classic()


# Calculate the middle value
middle_value <- median(ofc$X12)
ofc$bin <- as.integer(ofc$X12 >= middle_value)


lpfc_shape <- ofc[ofc$X11 == 'shape', ]
lpfc_color <- ofc[ofc$X11 == 'color', ]

lpfc_shape <- lpfc_shape %>% group_by(X1,X5) %>% summarise(avg_power=mean(X4)) %>% as.data.frame()
final_lpfc_shape <- lpfc_shape %>% group_by(X1) %>% summarise(final_avg=mean(avg_power),se=sd(avg_power)/sqrt(n()))  %>% as.data.frame()
final_lpfc_shape$condition <- c("shape")



lpfc_color <- lpfc_color %>% group_by(X1,X5) %>% summarise(avg_power=mean(X4)) %>% as.data.frame()
final_lpfc_color <- lpfc_color %>% group_by(X1) %>% summarise(final_avg=mean(avg_power),se=sd(avg_power)/sqrt(n()))  %>% as.data.frame()
final_lpfc_color$condition <- c("color")


lpfc_condition_master <- rbind(final_lpfc_shape,final_lpfc_color)




rolling_mean_shape <- rollapply(final_lpfc_shape$final_avg, width = 50, FUN = mean, by.column = FALSE, align = "right")
rolling_se_shape <- rollapply(final_lpfc_shape$se, width = 50, FUN = mean, by.column = FALSE, align = "right")

rolling_shape <- as.data.frame(rolling_mean_shape)
rolling_shape$se <- c(rolling_se_shape)
rolling_shape$condition <- c("shape")
rolling_shape$time <- c(1:length(rolling_mean_shape))

rolling_mean_color <- rollapply(final_lpfc_color$final_avg, width = 50, FUN = mean, by.column = FALSE, align = "right")
rolling_se_color <- rollapply(final_lpfc_color$se, width = 50, FUN = mean, by.column = FALSE, align = "right")

rolling_color <- as.data.frame(rolling_mean_color)
rolling_color$se <- c(rolling_se_color)
rolling_color$condition <- c("color")
rolling_color$time <- c(1:length(rolling_mean_color))


colnames(rolling_shape) <- c("mean","se","condition","time")
colnames(rolling_color) <- c("mean","se","condition","time")
lpfc_smoothed_master <- rbind(rolling_shape,rolling_color)

lpfc_smoothed_master %>%
  ggplot( aes(x=time, y=mean, color=condition,group=condition)) +
  geom_line()+ 
  geom_vline(xintercept = 750, col = "black",linetype = "dashed") + theme_classic() +  
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se,fill=condition),alpha = 0.5,color=NA,) 


setwd('/Users/christinamaher/Desktop/fellowship_data/')
write.csv(good_ofc_df, 'ofc_single_elec_plot.csv')


library(zoo)

ofc_high <- ofc[ofc$bin == 1, ]
ofc_low <- ofc[ofc$bin == 0, ]

ofc_high <- ofc_high %>% group_by(X1,X5) %>% summarise(avg_power=mean(X4)) %>% as.data.frame()
final_ofc_high <- ofc_high %>% group_by(X1) %>% summarise(final_avg=mean(avg_power),se=sd(avg_power)/sqrt(n()))  %>% as.data.frame()
final_ofc_high$condition <- c("high")



ofc_low <- ofc_low %>% group_by(X1,X5) %>% summarise(avg_power=mean(X4)) %>% as.data.frame()
final_ofc_low <- ofc_low %>% group_by(X1) %>% summarise(final_avg=mean(avg_power),se=sd(avg_power)/sqrt(n()))  %>% as.data.frame()
final_ofc_low$condition <- c("low")


ofc_ev_master <- rbind(final_ofc_high,final_ofc_low)




rolling_mean_high <- rollapply(final_ofc_high$final_avg, width = 50, FUN = mean, by.column = FALSE, align = "right")
rolling_se_high <- rollapply(final_ofc_high$se, width = 50, FUN = mean, by.column = FALSE, align = "right")

rolling_high <- as.data.frame(rolling_mean_high)
rolling_high$se <- c(rolling_se_high)
rolling_high$condition <- c("high")
rolling_high$time <- c(1:length(rolling_mean_high))

rolling_mean_low <- rollapply(final_ofc_low$final_avg, width = 50, FUN = mean, by.column = FALSE, align = "right")
rolling_se_low <- rollapply(final_ofc_low$se, width = 50, FUN = mean, by.column = FALSE, align = "right")

rolling_low <- as.data.frame(rolling_mean_low)
rolling_low$se <- c(rolling_se_low)
rolling_low$condition <- c("low")
rolling_low$time <- c(1:length(rolling_mean_low))


colnames(rolling_high) <- c("mean","se","condition","time")
colnames(rolling_low) <- c("mean","se","condition","time")
ofc_smoothed_master <- rbind(rolling_high,rolling_low)

ofc_smoothed_master %>%
  ggplot( aes(x=time, y=mean, color=condition,group=condition)) +
  geom_line()+ 
  geom_vline(xintercept = 750, col = "black",linetype = "dashed") + theme_classic() +  
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se,fill=condition),alpha = 0.5,color=NA,) 

setwd('/Users/christinamaher/Desktop/fellowship_data/')
write.csv(lpfc_smoothed_master, 'lpfc_grouped_lineplot.csv')

# Perform a rolling paired t-test
window_size = 50
pval_list <- c()
for (i in 1:(length(final_lpfc_shape$final_avg) - window_size + 1)) {
  window_group1 <- final_lpfc_shape$final_avg[i:(i + window_size - 1)]
  window_group2 <- final_lpfc_color$final_avg[i:(i + window_size - 1)]
  
  ttest_result <- t.test(window_group1, window_group2, paired = TRUE)
  
  p_value <- ttest_result$p.value
  pval_list <- c(pval_list, p_value)
}
adjustedpvalue <- 0.05/length(pval_list)
sig <- pval_list < adjustedpvalue

df <- data.frame(pval_list,sig)
setwd('/Users/christinamaher/Desktop/fellowship_data/')
write.csv(df, 'lpfc_pval_list.csv')

# Example data for two sets of values
regressors <- c("Reward", "ΦEV", "ΦRPE", "Relevant dimension")
ofc_bic <- c(-3329.717,-3349.994,-3330.639,-3334.682)
lpfc_bic <- c(1191.361,1117.542,1206.585,1102.11)

ofc_bic_df <- data.frame(regressors,ofc_bic)
library(tidyverse)


# Data
regressors <- c("Reward", "ΦEV", "ΦRPE", "Relevant dimension")
ofc_bic <- c(-3329.717, -3349.994, -3330.639, -3334.682)
mirror <- c(3309.44,3329.717,3310.362,3314.405)

# Create a data frame from the provided data
data <- data.frame(regressors, mirror)

# Create a barplot
# Create a barplot
# Set the y-axis limits to start from -3500

ggplot(data,aes(x=regressors,y=mirror)) + geom_bar(stat='identity') + ylim()
