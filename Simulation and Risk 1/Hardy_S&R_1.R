# read in libraries
library(tidyverse)
library(readxl)
library(quantmod)
library(graphics)
library(ks)
library(gtools)
library(triangle)

# goal: simulate possible future values of 2023 drilling costs

# read in data
# cost per well of crude oil, natural gas, and dry well
df <- read_excel("/Users/hardikag/Desktop/Spring_1-Blue_14/Simulation and Risk 1/Analysis_Data.xlsx", sheet = 2, skip = 2)

# subset to 1990 to 2006
df$Date <- format(as.Date(df$Date, format="%d/%m/%Y"),"%Y")
df_sub <- df[df$Date >= "1990" & df$Date <= "2006", ]

# convert all return changes to numeric
df_sub[,5] <- as.numeric(unlist(df_sub[,5]))
df_sub[,6] <- as.numeric(unlist(df_sub[,6]))
df_sub[,7] <- as.numeric(unlist(df_sub[,7]))

# simulate possible annual changes to get to 2023
# assume an average cost applies to all types of well
df_sub$cost_avg <- rowMeans(df_sub[,2:4])
df_sub$change_avg <- rowMeans(df_sub[,5:7])

##### kernel estimation ####################################################################################################

# use density function to find bandwith
change_density <- density(df_sub$change_avg)

# estimate kernel and plot
est_change <- rkde(fhat=kde(df_sub$change_avg, h=change_density$bw), n=5000)
hist(est_change, breaks=50, main='KDE of Annual Changes', xlab='Annual Change')

shapiro.test(est_change)
# conclusion, no, the changes from 2006 to 20012 do NOT follow a normal distribution ###################################

########## 2006 to 2023 ################################################################################################

# simulate 6 years into the future, 2006 - 2012
n = 5000 #number of simulations to run parameter

xbar = mean(df_sub$change_avg)
std_dev = sd(df_sub$change_avg)

P_06 <- last(df_sub$cost_avg) 

kde_23  <- rep()
n_23    <- rep()
set.seed(123)
for(i in 1:n) {
  P_07_12_kde <- 1 + rkde(fhat=kde(df_sub$change_avg, h=change_density$bw), n=6) # 6 value from the kernel
  P_07_12_n   <- 1 + rnorm(n=6, mean=xbar, sd=std_dev) # 6 value from the normal
  
  P_12_15 <- 1 + rtriangle(n=3, a=-0.22, b=-0.07, c=-0.0917)
  
  P_15_23 <- 1 + rtriangle(n=8, a=0.02, b=0.06, c=0.05)
  
  P_23_kde = P_06 * prod(P_07_12_kde) * prod(P_12_15) * prod(P_15_23)
  kde_23 <- append(kde_23, P_23_kde) 
  
  P_23_n   = P_06 * prod(P_07_12_n)   * prod(P_12_15) * prod(P_15_23)
  n_23 <- append(n_23, P_23_n) 
}
  
hist(kde_23)
hist(n_23)
