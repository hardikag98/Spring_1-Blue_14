# read in libraries
library(tidyverse)
library(readxl)
library(quantmod)
library(graphics)
library(ks)
library(gtools)
library(triangle)
library(ggplot2)
library(truncnorm)
library(scales)

set.seed(03031998)

#start <- Sys.time()
# read in data
# cost per well of crude oil, natural gas, and dry well
df <- read_excel("/Users/Jane/Documents/School/IAA/Classwork/Analytics 503- Optimization/Spring_1-Blue_14/Simulation and Risk 3/Analysis_Data.xlsx", sheet = 2, skip = 2)

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

########## 2006 to 2023 ################################################################################################

# simulate 6 years into the future, 2006 - 2012
n = 20000 #number of simulations to run parameter
set.seed(123)

xbar = mean(norm_change)
std_dev = sd(norm_change)

P_06 <- last(df_sub$cost_avg) 

n_23    <- rep()
for(i in 1:n) {
  P_07_12_n   <- 1 + rnorm(n=6, mean=xbar, sd=std_dev) # 6 value from the normal
  
  P_12_15 <- 1 + rtriangle(n=3, a=-0.22, b=-0.07, c=-0.0917)
  
  P_15_23 <- 1 + rtriangle(n=8, a=0.02, b=0.06, c=0.05)
  
  P_23_n   = P_06 * prod(P_07_12_n)   * prod(P_12_15) * prod(P_15_23)
  n_23 <- append(n_23, P_23_n) 
}

# plotting histograms
# hist(n_23)

# scale data 
n_23 <- n_23 * 1000

##Phase 2---------------
data <- read_excel("/Users/Jane/Documents/School/IAA/Classwork/Analytics 503- Optimization/Spring_1-Blue_14/Simulation and Risk 3/Analysis_Data.xlsx", sheet = 1, skip = 2)
#colnames(data) <- data[2,]
#data <- data[3:nrow(data),]

data$Year <- as.numeric(data$Year)
data$`High Oil Price`<- as.numeric(data$`High Oil Price`)
data$`Low Oil Price`<- as.numeric(data$`Low Oil Price`)
data$`AEO2021 Reference`<- as.numeric(data$`AEO2021 Reference`)

data <- data %>% mutate(across(is.numeric, round, digits=5))
str(data)

sim_size <- n

#### Phase 3 code
# Hydrocarbons
hydrocarbon <- rtruncnorm(sim_size, a=0, b=1, mean=.99, sd=.05)

######### save distribution of hydrocarbon

saveRDS(hydrocarbon, 
        file = "/Users/Jane/Documents/School/IAA/Classwork/Analytics 503- Optimization/Spring_1-Blue_14/Simulation and Risk 3/hydrocarbon_JW2.rds")

#hist(hydrocarbon)


# Reservoir
reservoir <- rtruncnorm(sim_size, a=0, b=1, mean=.8, sd=.1)

############# save distribution of reservoir

saveRDS(reservoir, 
        file = "/Users/Jane/Documents/School/IAA/Classwork/Analytics 503- Optimization/Spring_1-Blue_14/Simulation and Risk 3/reservoir_JW2.rds")

#hist(reservoir)

# structure and seal
structure <- rep(1, sim_size)
seal <- rep(1, sim_size)

# probability of a producing well
prob_produce <- hydrocarbon * reservoir * structure * seal
#hist(prob_produce)

# number of planned wells, comes from a uniform distribution
planned_wells <- rdunif(sim_size, 10, 30)
# hist(planned_wells)
# barplot(table(rdunif(sim_size, 10, 30))) 


# randomly sample number of planned wells from the probability of producing wells

prod_wells <- data.frame(matrix(ncol = 3, nrow = 0))# create an empty list
colnames(prod_wells) <- c('well_ID','producing', 'cost_NPV')


for (i in index(planned_wells)) { # iterate through the number of planned wells
  id_well <- i
  #print(id_well)
  for (j in 1:planned_wells[i]) {
    producing = rbinom(1, 1, sample(prob_produce,j,replace=FALSE)) #replace to FALSE for full simulation
    prod_wells[nrow(prod_wells) + 1,] = c(id_well, producing, 0)
  }
  # producing = sum(rbinom(j, 1, sample(prob_produce,j,replace=TRUE))) 
  # dry = j -  sum(rbinom(j, 1, sample(prob_produce,j,replace=TRUE))) 
  # prod_wells[nrow(prod_wells) + 1,] = c(id_well, dry, producing)
  # gives number of producing wells
}
#Functions needed
standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

# now can calculate NPV and costs
for (i in index(prod_wells)){
  # need to go row by row, if 0 then dry else 1
  
  if (prod_wells[i, 2] == 0){ # dry wells, just year 0 costs
    leased_acres <- rnorm(n=n, mean=600, sd=50)
    leased_acres_cost <- 960*leased_acres
    
    seismic_sections <- rnorm(n=n, mean=3, sd=0.35)
    seismic_sections_cost <- 43000*seismic_sections
    
    oil_prod_cost <- rnorm(n=n, mean=390000, sd=50000)
    
    team_cost <- rtriangle(n=n, a=172000, b=279500, c=215000)
    
    # year_0_costs <- n_23 + leased_acres_cost + seismic_sections_cost + 
    #   oil_prod_cost + team_cost
    
    year_0_dry_costs <- (n_23 + leased_acres_cost + seismic_sections_cost + team_cost)* -1
    prod_wells$cost_NPV[i] <- list(year_0_dry_costs)
  }
  
  if (prod_wells[i, 2] == 1) { #prod wells
    # Year 0 Costs #
    
    leased_acres <- rnorm(n=n, mean=600, sd=50)
    leased_acres_cost <- 960*leased_acres
    
    seismic_sections <- rnorm(n=n, mean=3, sd=0.35)
    seismic_sections_cost <- 43000*seismic_sections
    
    oil_prod_cost <- rnorm(n=n, mean=390000, sd=50000)
    
    
    team_cost <- rtriangle(n=n, a=172000, b=279500, c=215000)
    
    
    year_0_costs <- n_23 + leased_acres_cost + seismic_sections_cost +
      oil_prod_cost + team_cost
    
    #Correlation Between Simulations
    R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
    U <- t(chol(R))
    
    #Set up and Simulation of the LogNormal Distribution, 420 mean, 120 std dev
    loc <- log(420^2 / sqrt(120^2 + 420^2))
    shp <- sqrt(log(1 + (120^2 / 420^2)))
    year_start_rate <- rlnorm(n=n, meanlog = loc, sdlog = shp)
    #hist(year_start_rate)
    
    
    #First year rate of decline simulation
    rod <- runif(n=n, min = 0.15, max = 0.32)
    
    #Factor in correlation
    Both <- cbind(standardize(year_start_rate), standardize(rod))
    rod_id_corr <- U %*% t(Both)
    rod_id_corr <- t(rod_id_corr)
    
    all_sims <- cbind(destandardize(rod_id_corr[,1], year_start_rate),
                      destandardize(rod_id_corr[,2], rod))
    
    cor(all_sims[,1],all_sims[,2])
    
    all_sims <- as.data.frame(all_sims)
    ysr <- all_sims$V1 #year start rate
    rod <- all_sims$V2
    
    oil_volume <- NULL
    #Calculating the volume for each year based on rate of decline
    for(k in 1:15){
      year_end_rate <- (1-rod)*ysr
      oil_volume[[k]] <- 365*((year_end_rate+ysr)/2)
      ysr <- year_end_rate
    }
    
    
    #Calculating Oil Value from long term forecasts
    oil_value <- NULL
    operating_cost <- NULL
    for(k in 1:15){
      oil_value[[k]] <- rtriangle(n=n,
                                  a=data$`Low Oil Price`[k],
                                  b=data$`High Oil Price`[k],
                                  c=data$`AEO2021 Reference`[k])
      operating_cost[[k]] <- rnorm(n=n, mean=2.25, sd= 0.3)
    }
    
    
    #Net Revenue Interest
    NRI <- rnorm(n=n, mean=0.75, sd=0.02)
    
    #Barrel cost and profit
    total_barrel_cost <- NULL
    total_rev <- NULL
    sev_taxes <- NULL #JW added additional operating expense
    for(k in 1:15){
      total_rev[[k]] <- oil_volume[[k]]*oil_value[[k]]*NRI
      total_barrel_cost[[k]] <- oil_volume[[k]]*operating_cost[[k]]
      sev_taxes[[k]] <- total_rev[[k]] * 0.046 # JW added additional operating expense
    }
    
    #Loop for expenses for each year
    yearly_expenses <- NULL
    yearly_profit <- NULL
    for(k in 1:15){
      yearly_expenses[[k]] <-  team_cost + total_barrel_cost[[k]] + sev_taxes[[k]]
      yearly_profit[[k]] <- total_rev[[k]]-yearly_expenses[[k]]
      #print(quantile(profit[[i]]))
    }
    
    profit <- -1*year_0_costs
    WACC <- 0.1
    for(k in 1:15){
      profit <- profit + (yearly_profit[[k]]/((1+WACC)^k))
    }
    prod_wells$cost_NPV[i] <- list(profit)
  }
}

# find number of dry and wet wells
dry_prod_wells <- prod_wells %>% group_by(well_ID) %>% summarise(prod = sum(producing), dry = length(producing) - sum(producing))


########## save distribution of proportion of producing and dry wells
saveRDS(dry_prod_wells, 
        file = "/Users/Jane/Documents/School/IAA/Classwork/Analytics 503- Optimization/Spring_1-Blue_14/Simulation and Risk 3/dry_prod_wells_JW2.rds")


# distribution of proportion of wet wells
hist(dry_prod_wells$prod / (dry_prod_wells$prod + dry_prod_wells$dry))

# now, need to group by wells and unpack to get distributions to get VaR and CVaR
wells_var <- prod_wells %>% group_by(well_ID) %>% summarise(cost_NPV = list(cost_NPV))

for (i in index(wells_var$cost_NPV)) {
  wells_var$cost_NPV[i] <- list(unlist(wells_var$cost_NPV[i]))
}


# now unpack into one big distribution (used to calculate VaR and CVaR)
wells_dist <- unlist(wells_var$cost_NPV)


######### save simulated data
saveRDS(wells_dist, 
        file = "/Users/Jane/Documents/School/IAA/Classwork/Analytics 503- Optimization/Spring_1-Blue_14/Simulation and Risk 3/wells_dist_JW2.rds")

# test = readRDS(file = "/Users/Jane/Documents/School/IAA/Classwork/Analytics 503- Optimization/Spring_1-Blue_14/Simulation and Risk 3/wells_dist_JW.rds")

hist(wells_dist)

# expected return
dollar(mean(wells_dist))

# calc VaR
VaR_percentile <- .05

VaR <- quantile(wells_dist, VaR_percentile, na.rm=TRUE)
dollar(VaR)

# calc CVaR
ES <- mean(wells_dist[wells_dist < VaR], na.rm=TRUE)
dollar(ES)

#print( Sys.time() - start )

# ggplot() + 
#   aes(profit) + 
#   geom_histogram(colour="black", fill="light blue")+
#   xlab("Profit") +
#   ylab("Frequency") +
#   scale_x_continuous(labels=scales::dollar_format()) +
#   scale_y_continuous(labels = scales::comma) +
#   ggtitle("Distribution of 1,000,000 Simulations of Profit of a Single Wet Well") +
#   theme(
#     panel.background = element_rect(fill='transparent'), #transparent panel bg
#     plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
#     panel.grid.major = element_blank(), #remove major gridlines
#     panel.grid.minor = element_blank(), #remove minor gridlines
#   )
# 
# ggplot() + 
#   aes(year_0_dry_costs) + 
#   geom_histogram(colour="black", fill="light blue")+
#   xlab("Cost") +
#   ylab("Frequency") +
#   scale_x_continuous(labels=scales::dollar_format()) +
#   scale_y_continuous(labels = scales::comma) +
#   ggtitle("Distribution of 1,000,000 Simulations of Cost of a Single Dry Well") +
#   theme(
#     panel.background = element_rect(fill='transparent'), #transparent panel bg
#     plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
#     panel.grid.major = element_blank(), #remove major gridlines
#     panel.grid.minor = element_blank(), #remove minor gridlines
#   )

# quantile(year_0_dry_costs, probs = c(0.01,0.05,0.25,0.5,0.95,0.99))
# quantile(profit, probs = c(0.01,0.05,0.25,0.5,0.95,0.99))
# mean(year_0_dry_costs)
# mean(profit)

# #Evaluate negative NPV cases
# # profit is how much each well makes
# prof <- as.data.frame(profit) 
# prof$id <- seq(1,n,1)
# prof_neg <- prof %>% filter(profit <= 0)
# #726/10000
# # negative profit is rare (exceptionally high labor cost and low production)
# 
# #Exploring why is the worst case the worst case
# # year_0_costs[836616]
# # team_cost[836616]
# # leased_acres[836616]
# # seismic_sections[836616]
# # n_23[836616]
# # n23 <- as.data.frame(n_23)
# # for(i in 1:15){
# #   print(oil_value[[i]][836616])
# #   print(oil_volume[[i]][836616])
# # }
# # ysr_df <- as.data.frame(all_sims$V1)
# # ysr_df$`all_sims$V1`[836616]
# 
# 
# # Expected Shortfall
# y0dc <- as.data.frame(year_0_dry_costs)
# y0dc_1perc <- quantile(year_0_dry_costs, probs = c(0.99))[1]
# worst_dry <- y0dc %>% filter(year_0_dry_costs > y0dc_1perc)
# es_dry <- mean(worst_dry$year_0_dry_costs)
# es_dry
# 
# 
# wet_1perc <- quantile(profit,probs = c(0.01))[1]
# worst_wet <- prof %>% filter(profit < wet_1perc)
# es_wet <- mean(worst_wet$profit)
# es_wet
