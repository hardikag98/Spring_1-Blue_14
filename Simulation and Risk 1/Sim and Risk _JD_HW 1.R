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
df <- read_excel("C:\\Users\\Justin\\OneDrive - North Carolina State University\\Documents\\NC State\\IAA R\\Data\\Analysis_Data.xlsx", sheet = 2, skip = 2)

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
est_change <- rkde(fhat=kde(df_sub$change_avg, h=change_density$bw), n=50000)
hist(est_change, breaks=50, main='KDE of Annual Changes', xlab='Annual Change')


########## 2006 to 2012, KDF, test for normality #########################################################################

# simulate 6 years into the future, 2006 - 2012
n = 10 #number of simulations to run parameter
P_07 <- rep() # vector to put results in
P_08 <- rep()
P_09 <- rep()
P_10 <- rep()
P_11 <- rep()
P_12 <- rep()
P_13 <- rep() # vector to put results in
P_14 <- rep()
P_15 <- rep()
P_16 <- rep() # vector to put results in
P_17 <- rep()
P_18 <- rep()
P_19 <- rep()
P_20 <- rep()
P_21 <- rep()
P_22 <- rep()
P_23 <- rep()


for(i in 1:n) {
  P0 <- last(df_sub$change_avg) # use the last annual change from 2006 as the base
  r <- rkde(fhat=kde(df_sub$change_avg, h=change_density$bw), n=1) # 1 value from the kernel
  P7 <- P0*(1 + r) # multiply by change
  P_07 <- append(P_07, P7)
  
  r <- rkde(fhat=kde(df_sub$change_avg, h=change_density$bw), n=1) # 1 value from the kernel
  P8 <- P7*(1 + r) # multiply by change
  P_08 <- append(P_08, P8)
  
  r <- rkde(fhat=kde(df_sub$change_avg, h=change_density$bw), n=1) # 1 value from the kernel
  P9 <- P8*(1 + r) # multiply by change
  P_09 <- append(P_09, P9)
  
  r <- rkde(fhat=kde(df_sub$change_avg, h=change_density$bw), n=1) # 1 value from the kernel
  P10 <- P9*(1 + r) # multiply by change
  P_10 <- append(P_10, P10)
  
  r <- rkde(fhat=kde(df_sub$change_avg, h=change_density$bw), n=1) # 1 value from the kernel
  P11 <- P10*(1 + r) # multiply by change
  P_11 <- append(P_11, P11)
  
  r <- rkde(fhat=kde(df_sub$change_avg, h=change_density$bw), n=1) # 1 value from the kernel
  P12 <- P11*(1 + r) # multiply by change
  P_12 <- append(P_12, P12)
  
  # 
  r <- rtriangle(n=1, a=7, b=22, c=9.17) * -.01# 1 value from the kernel
  P13 <- P12*(1 + r) # multiply by change
  P_13 <- append(P_13, P13)
  
  r <- rtriangle(n=1, a=7, b=22, c=9.17) * -.01# 1 value from the kernel
  P14 <- P13*(1 + r) # multiply by change
  P_14 <- append(P_14, P14)
  
  r <- rtriangle(n=1, a=7, b=22, c=9.17) * -.01# 1 value from the kernel
  P15 <- P14*(1 + r) # multiply by change
  P_15 <- append(P_15, P15)
  
  #
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P16 <- P15*(1 + r) # multiply by change
  P_16 <- append(P_16, P16)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P17 <- P16*(1 + r) # multiply by change
  P_17 <- append(P_17, P17)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P18 <- P17*(1 + r) # multiply by change
  P_18 <- append(P_18, P18)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P19 <- P18*(1 + r) # multiply by change
  P_19 <- append(P_19, P19)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P20 <- P19*(1 + r) # multiply by change
  P_20 <- append(P_20, P20)
  
  r <-rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P21 <- P20*(1 + r) # multiply by change
  P_21 <- append(P_21, P21)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P22 <- P21*(1 + r) # multiply by change
  P_22 <- append(P_22, P22)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P23 <- P22*(1 + r) # multiply by change
  P_23 <- append(P_23, P23)
}



########## 2006 to 2012, Normality assumption, test for normality #########################################################################

xbar = mean(df_sub$change_avg)
std_dev = sd(df_sub$change_avg)
# simulate 6 years into the future, 2006 - 2012
n = 10 #number of simulations to run parameter
P_07_n <- rep() # vector to put results in
P_08_n <- rep()
P_09_n <- rep()
P_10_n <- rep()
P_11_n <- rep()
P_12_n <- rep()
P_13_n <- rep() # vector to put results in
P_14_n <- rep()
P_15_n <- rep()
P_16_n <- rep() # vector to put results in
P_17_n <- rep()
P_18_n <- rep()
P_19_n <- rep()
P_20_n <- rep()
P_21_n <- rep()
P_22_n <- rep()
P_23_n <- rep()

for(i in 1:n) {
  P0_n <- last(df_sub$change_avg) # use the last annual change from 2006 as the base
  r <- rnorm(n=1, mean=xbar, sd=std_dev) # 1 value from the kernel
  P7_n <- P0_n*(1 + r) # multiply by change
  P_07_n <- append(P_07_n, P7_n)
  
  r <- rnorm(n=1, mean=xbar, sd=std_dev)  # 1 value from the kernel
  P8_n <- P7_n*(1 + r) # multiply by change
  P_08_n <- append(P_08_n, P8_n)
  
  r <- rnorm(n=1, mean=xbar, sd=std_dev)  # 1 value from the kernel
  P9_n <- P8_n*(1 + r) # multiply by change
  P_09_n <- append(P_09_n, P9_n)
  
  r <- rnorm(n=1, mean=xbar, sd=std_dev)  # 1 value from the kernel
  P10_n <- P9_n*(1 + r) # multiply by change
  P_10_n <- append(P_10_n, P10_n)
  
  r <- rnorm(n=1, mean=xbar, sd=std_dev)  # 1 value from the kernel
  P11_n <- P10_n*(1 + r) # multiply by change
  P_11_n <- append(P_11_n, P11_n)
  
  r <- rnorm(n=1, mean=xbar, sd=std_dev)  # 1 value from the kernel
  P12_n <- P11_n*(1 + r) # multiply by change
  P_12_n <- append(P_12_n, P12_n)
  
  #
  r <- rtriangle(n=1, a=7, b=22, c=9.17) * -.01# 1 value from the kernel
  P13_n <- P12_n*(1 + r) # multiply by change
  P_13_n <- append(P_13_n, P13_n)
  
  r <- rtriangle(n=1, a=7, b=22, c=9.17) * -.01# 1 value from the kernel
  P14_n <- P13_n*(1 + r) # multiply by change
  P_14_n <- append(P_14_n, P14_n)
  
  r <- rtriangle(n=1, a=7, b=22, c=9.17) * -.01# 1 value from the kernel
  P15_n <- P14_n*(1 + r) # multiply by change
  P_15_n <- append(P_15_n, P15_n)
  
  #
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P16_n <- P15_n*(1 + r) # multiply by change
  P_16_n <- append(P_16_n, P16_n)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P17_n <- P16*(1 + r) # multiply by change
  P_17_n <- append(P_17_n, P17_n)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P18_n <- P17_n*(1 + r) # multiply by change
  P_18_n <- append(P_18_n, P18_n)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P19_n <- P18_n*(1 + r) # multiply by change
  P_19_n <- append(P_19_n, P19_n)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P20_n <- P19_n*(1 + r) # multiply by change
  P_20_n <- append(P_20_n, P20_n)
  
  r <-rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P21_n <- P20_n*(1 + r) # multiply by change
  P_21_n <- append(P_21_n, P21_n)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P22_n <- P21_n*(1 + r) # multiply by change
  P_22_n <- append(P_22_n, P22_n)
  
  r <- rtriangle(n=1, a=2, b=6, c=5) * .01# 1 value from the kernel
  P23_n <- P22_n*(1 + r) # multiply by change
  P_23_n <- append(P_23_n, P23_n)
}

###############2023 Predictions#############################################################################################

summary(P_23)

summary(P_23_n)
