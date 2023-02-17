library(quantmod)
library(graphics)
library(ks)
library(triangle)
library(readxl)
library(tidyverse)

data <- read_xlsx("Analysis_Data.xlsx")
colnames(data) <- data[2,]
data <- data[3:nrow(data),]

data$Year <- as.numeric(data$Year)
data$`High Oil Price`<- as.numeric(data$`High Oil Price`)
data$`Low Oil Price`<- as.numeric(data$`Low Oil Price`)
data$`AEO2021 Reference`<- as.numeric(data$`AEO2021 Reference`)

data <- data %>% mutate(across(is.numeric, round, digits=5))
str(data)
years <- nrow(data)


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


year_0_costs <- leased_acres_cost + seismic_sections_cost + 
                oil_prod_cost + team_cost
hist(year_0_costs)
quantile(year_0_costs)


year_0_dry_costs <- leased_acres_cost + seismic_sections_cost + team_cost
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
for(i in 1:years){
  total_rev[[i]] <- oil_volume[[i]]*oil_value[[i]]*NRI
  total_barrel_cost[[i]] <- oil_volume[[i]]*operating_cost[[i]]
  sev_taxes[[i]] <- total_rev[[i]] * 0.046 # JW added additional operating expense
}

