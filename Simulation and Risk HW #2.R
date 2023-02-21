#Drilling Costs From Phase 1--------------
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
df <- read_excel("Analysis_Data.xlsx", sheet = 2, skip = 2)

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
est_change <- rkde(fhat=kde(norm_change, h=change_density$bw), n=1000000)
hist(est_change, breaks=50, main='KDE of Annual Changes', xlab='Annual Change')

ks.test(est_change, 'pnorm')
# conclusion, no, the changes from 2006 to 20012 do NOT follow a normal distribution ###################################

########## 2006 to 2023 ################################################################################################

# simulate 6 years into the future, 2006 - 2012
n = 1000000 #number of simulations to run parameter

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
###-------------------------------------

##Phase 2---------------
data <- read_xlsx("Analysis_Data.xlsx")
colnames(data) <- data[2,]
data <- data[3:nrow(data),]

data$Year <- as.numeric(data$Year)
data$`High Oil Price`<- as.numeric(data$`High Oil Price`)
data$`Low Oil Price`<- as.numeric(data$`Low Oil Price`)
data$`AEO2021 Reference`<- as.numeric(data$`AEO2021 Reference`)

data <- data %>% mutate(across(is.numeric, round, digits=5))
str(data)


# Year 0 Costs #
set.seed(17)
leased_acres <- rnorm(n=1000000, mean=600, sd=50)
leased_acres_cost <- 960*leased_acres
set.seed(17)
seismic_sections <- rnorm(n=1000000, mean=3, sd=0.35)
seismic_sections_cost <- 43000*seismic_sections


set.seed(17)
oil_prod_cost <- rnorm(n=1000000, mean=390000, sd=50000)


set.seed(17)
team_cost <- rtriangle(n=1000000, a=172000, b=279500, c=215000)

#If there is no oil then no production or team costs

#oil_prod_cost <- NULL
#team_cost_year_1 <- NULL
#for(i in 1:length(team_cost)){
  #oil_prod_cost[i] <- ifelse(oil_pres[i] == 1, oil_prod_cost[i], 0)
  #team_cost_year_1[i] <- ifelse(oil_pres[i] == 1, team_cost[i], 0)
#}


year_0_costs <- n_23 + leased_acres_cost + seismic_sections_cost + 
                oil_prod_cost + team_cost
hist(year_0_costs)
quantile(year_0_costs)


year_0_dry_costs <- n_23 + leased_acres_cost + seismic_sections_cost + team_cost
hist(year_0_dry_costs)
quantile(year_0_dry_costs)


#Year Costs

#Correlation Between Simulations
R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
U <- t(chol(R))

#Functions needed
standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

#Set up and Simulation of the LogNormal Distribution
loc <- log(420^2 / sqrt(120^2 + 420^2))
shp <- sqrt(log(1 + (120^2 / 420^2)))
set.seed(17)
year_start_rate <- rlnorm(n=1000000, meanlog = loc, sdlog = shp)
hist(year_start_rate)

cor(all_sims$V1,all_sims$V2)
#Is 0.640

#First year rate of decline simulation
#rod <- NULL
set.seed(17)
rod <- runif(n=1000000, min = 0.15, max = 0.32)

#Factor in correlation
Both <- cbind(standardize(year_start_rate), standardize(rod))
rod_id_corr <- U %*% t(Both)
rod_id_corr <- t(rod_id_corr)

all_sims <- cbind(destandardize(rod_id_corr[,1], year_start_rate),
                    destandardize(rod_id_corr[,2], rod))


#Remaining years rate of decline simulations 
#(Is same across years but not wells so incorrect?)

#for(i in 2:years){
  
#  set.seed(17*i)
#  rod[[i]] <- runif(n=1000000, min = 0.15, max = 0.32)
  
#  Both <- cbind(standardize(year_start_rate), standardize(rod[[i]]))
#  SB.r <- U %*% t(Both)
#  SB.r <- t(SB.r)
  
#  current_sim <- cbind(destandardize(SB.r[,1], year_start_rate),
#                      destandardize(SB.r[,2], rod[[i]]))
#  current_sim <- as.data.frame(current_sim)
#  all_sims <- cbind(all_sims,current_sim$V2)
#}



all_sims <- as.data.frame(all_sims)
ysr <- all_sims$V1 #year start rate
rod <- all_sims$V2

oil_volume <- NULL
#Calculating the volume for each year based on rate of decline
for(i in 1:15){
  year_end_rate <- (1-rod)*ysr
  oil_volume[[i]] <- 365*((year_end_rate+ysr)/2)
  ysr <- year_end_rate
}


#Calculating Oil Value from long term forecasts
oil_value <- NULL
operating_cost <- NULL
for(i in 1:15){
  set.seed(17*i)
  oil_value[[i]] <- rtriangle(n=1000000, 
                             a=data$`Low Oil Price`[i],
                             b=data$`High Oil Price`[i],
                             c=data$`AEO2021 Reference`[i])
  set.seed(17*i)
  operating_cost[[i]] <- rnorm(n=1000000, mean=2.25, sd= 0.3)
}


#Net Revenue Interest
NRI <- rnorm(n=1000000, mean=0.75, sd=0.02)

#Barrel cost and profit
total_barrel_cost <- NULL
total_rev <- NULL
sev_taxes <- NULL #JW added additional operating expense
for(i in 1:15){
  total_rev[[i]] <- oil_volume[[i]]*oil_value[[i]]*NRI
  total_barrel_cost[[i]] <- oil_volume[[i]]*operating_cost[[i]]
  sev_taxes[[i]] <- total_rev[[i]] * 0.046 # JW added additional operating expense
}

#Loop for expenses for each year
yearly_expenses <- NULL
yearly_profit <- NULL
for(i in 1:15){
  yearly_expenses[[i]] <-  team_cost + total_barrel_cost[[i]] + sev_taxes[[i]]
  yearly_profit[[i]] <- total_rev[[i]]-yearly_expenses[[i]]
  #print(quantile(profit[[i]]))
}

profit <- -1*year_0_costs
WACC <- 0.1
for(i in 1:15){
   profit <- profit + (yearly_profit[[i]]/((1+WACC)^i))
}
hist(profit)
quantile(profit)

ggplot() + 
  aes(profit) + 
  geom_histogram(colour="black", fill="light blue")+
  xlab("Profit") +
  ylab("Frequency") +
  scale_x_continuous(labels=scales::dollar_format()) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Distribution of 1,000,000 Simulations of Profit of a Single Wet Well") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
  )

ggplot() + 
  aes(year_0_dry_costs) + 
  geom_histogram(colour="black", fill="light blue")+
  xlab("Cost") +
  ylab("Frequency") +
  scale_x_continuous(labels=scales::dollar_format()) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Distribution of 1,000,000 Simulations of Cost of a Single Dry Well") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
  )

quantile(year_0_dry_costs, probs = c(0.01,0.05,0.25,0.5,0.95,0.99))
quantile(profit, probs = c(0.01,0.05,0.25,0.5,0.95,0.99))
mean(year_0_dry_costs)
mean(profit)

#Evaluate negative NPV cases
prof <- as.data.frame(profit)
prof$id <- seq(1,1000000,1)
prof_neg <- prof %>% filter(profit <= 0)
726/10000


#Exploring why is the worst case the worst case
year_0_costs[836616]
team_cost[836616]
leased_acres[836616]
seismic_sections[836616]
n_23[836616]
n23 <- as.data.frame(n_23)
for(i in 1:15){
  print(oil_value[[i]][836616])
  print(oil_volume[[i]][836616])
}
ysr_df <- as.data.frame(all_sims$V1)
ysr_df$`all_sims$V1`[836616]


# Expected Shortfall
y0dc <- as.data.frame(year_0_dry_costs)
y0dc_1perc <- quantile(year_0_dry_costs, probs = c(0.99))[1]
worst_dry <- y0dc %>% filter(year_0_dry_costs > y0dc_1perc)
es_dry <- mean(worst_dry$year_0_dry_costs)
es_dry


wet_1perc <- quantile(profit,probs = c(0.01))[1]
worst_wet <- prof %>% filter(profit < wet_1perc)
es_wet <- mean(worst_wet$profit)
es_wet
