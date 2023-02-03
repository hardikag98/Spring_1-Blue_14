# read in libraries
library(tidyverse)
library(readxl)
library(quantmod)
library(graphics)
library(ks)
library(gtools)
library(triangle)
library(ggplot2)

# goal: simulate possible future values of 2023 drilling costs

# read in data
# cost per well of crude oil, natural gas, and dry well
df <- read_excel("C:\\Users\\Justin\\OneDrive - North Carolina State University\\Documents\\NC State\\IAA R\\Data\\Analysis_Data.xlsx", sheet = 2, skip = 2)

# subset to 1990 to 2006
df$Date <- format(as.Date(df$Date, format="%d/%m/%Y"),"%Y")
df_sub <- df[df$Date > "1990" & df$Date <= "2006", ]

# convert all return changes to numeric
df_sub[,5] <- as.numeric(unlist(df_sub[,5]))
df_sub[,6] <- as.numeric(unlist(df_sub[,6]))
df_sub[,7] <- as.numeric(unlist(df_sub[,7]))

# simulate possible annual changes to get to 2023
# assume an average cost applies to all types of well


norm_change <- rep()
norm_change <- append(norm_change, df_sub$`Arithmetic Return - Crude Oil`)
norm_change <- append(norm_change, df_sub$`Arithmetic Return - Natural Gas`)
norm_change <- append(norm_change, df_sub$`Arithmetic Return - Dry Well`)


df_sub$cost_avg <- rowMeans(df_sub[,2:4])
df_sub$change_avg <- rowMeans(df_sub[,5:7])

##### kernel estimation ####################################################################################################

# use density function to find bandwidth
change_density <- density(norm_change)

# estimate kernel and plot
est_change <- rkde(fhat=kde(norm_change, h=change_density$bw), n=100000)
hist(est_change, breaks=50, main='KDE of Annual Changes', xlab='Annual Change')

ks.test(est_change, 'pnorm')
# conclusion, no, the changes from 2006 to 20012 do NOT follow a normal distribution ###################################

########## 2006 to 2023 ################################################################################################

# simulate 6 years into the future, 2006 - 2012
n = 100000 #number of simulations to run parameter

xbar = mean(norm_change)
std_dev = sd(norm_change)

P_06 <- last(df_sub$cost_avg) 

kde_23  <- rep()
n_23    <- rep()

set.seed(123)
for(i in 1:n) {
  P_07_12_kde <- 1 + rkde(fhat=kde(norm_change, h=change_density$bw), n=6) # 6 value from the kernel
  P_07_12_n   <- 1 + rnorm(n=6, mean=xbar, sd=std_dev) # 6 value from the normal
  
  P_12_15 <- 1 + rtriangle(n=3, a=-0.22, b=-0.07, c=-0.0917)
  
  P_15_23 <- 1 + rtriangle(n=8, a=0.02, b=0.06, c=0.05)
  
  P_23_kde = P_06 * prod(P_07_12_kde) * prod(P_12_15) * prod(P_15_23)
  kde_23 <- append(kde_23, P_23_kde) 
  
  P_23_n   = P_06 * prod(P_07_12_n)   * prod(P_12_15) * prod(P_15_23)
  n_23 <- append(n_23, P_23_n) 
}

# plotting histograms
hist(kde_23)
hist(n_23)

# scale data 
kde_23 <- kde_23 * 1000
n_23 <- n_23 * 1000

# 5 number summary
summary(kde_23)
summary(n_23)

bin_count = round(sqrt(n), 0)

# make nicer plots
ggplot(as.data.frame(kde_23), aes(x=kde_23)) + geom_histogram(bins=bin_count, colour="black", fill="grey")+
  geom_vline(aes(xintercept=mean(kde_23)), color="blue",
             linetype="dashed", size = 1)+ 
  scale_x_continuous(labels = scales::dollar)+
  labs(title="2023 Drilling Cost Under Kernel Density Estimate for 2006 – 2012",x="Driling Cost", y = "Count")+
  theme_classic()

ggplot(as.data.frame(n_23), aes(x=n_23)) + geom_histogram(bins=bin_count, colour="black", fill="grey")+
  geom_vline(aes(xintercept=mean(n_23)), color="blue",
             linetype="dashed", size = 1)+
  scale_x_continuous(labels = scales::dollar)+
  labs(title="2023 Drilling Cost Under Normality Assumption for 2006 – 2012",x="Driling Cost", y = "Count")+
  theme_classic()

ggplot(as.data.frame(est_change), aes(x=est_change)) + geom_histogram(bins=bin_count, colour="black", fill="grey")+
  geom_vline(aes(xintercept=mean(est_change)), color="blue",
             linetype="dashed", size = 1)+
  scale_x_continuous(labels = scales::percent)+
  labs(title="Kernel Density Estimate of Annual Changes",x="Change (%)", y = "Count")+
  theme_classic()
