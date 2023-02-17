# read in libraries
library(tidyverse)
library(readxl)
library(quantmod)
library(graphics)
library(ks)
library(gtools)
library(triangle)
library(ggplot2)

setwd("G:/My Drive/MSA/2022 Spring/AA 503/Simulation and Risk/Homework 2")

# read in data
# price projections for 2023 - 2050
projections <- read_excel("Analysis_Data.xlsx", sheet = 1, skip = 2)

num_sim = 1000 # number of simulations to run

# Revenue Risk
# Given have best, worst, and typical values, we need 
# use a Triangle distribution for this simulation.

# for each year build a triangle distribution
# sample each distribution num_sim times

# create data frame to save distribution simulations
proj_dist <- NULL

for (i in 1:nrow(projections)){
  # get values for the year
  year = projections$Year[i] #number
  minimum = projections$`Low Oil Price`[i]
  maximum = projections$`High Oil Price`[i]
  likely = projections$`AEO2021 Reference`[i]
  set.seed(17*num_sim) #set a changing seed number
  # run simulation num_sim times for each year
  oil_price <- rtriangle(n=num_sim, a=minimum, b=maximum, c=likely) #list
  # save results
  proj_dist = cbind(proj_dist, oil_price)
}
#rename the columns to be the years
col_names = c("2023", "2024", "2025", "2026", "2027", "2028", "2029",
              "2030", "2031", "2032", "2033", "2034", "2035", "2036",
              "2037", "2038", "2039", "2040", "2041", "2042", "2043",
              "2044", "2045", "2046", "2047", "2048", "2049", "2050")
colnames(proj_dist) <- col_names

# Simulation for oil prices
# Annual revenues = Oil Price * Annual Production 
# for each column, get the average oil price
avg_oil_price <- colMeans(proj_dist) #vector with named positions
######fake list of annual production###########################################
an_prod <- seq(1, 28, 1) #vector of values
###############################################################################
# multiply the avg oil price by annual production 
an_rev <- avg_oil_price * an_prod #vector with named positions


# Simulation for Net Revenue Interest (NRI)
# Revenue = Annual revenues * NRI
# NRI distributed Normally with a mean of 75% and a standard deviation of 2%

# This calculation is done per well for the entire life of the well

set.seed(17) #should seeds change every time????
nri <- rnorm(n=num_sim, mean=.75, sd=.02) #decimals since they are in percentages???


