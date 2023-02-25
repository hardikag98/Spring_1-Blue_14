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

# goal: get probability of producing a well(truncated normal) -> number of producing wells (uniform 10 - 30)
# run a simulation for number of wells and if those wells are wet or dry

# truncated normal distribution ##################################################################################
sim_size <- 5000

# Hydrocarbons
hydrocarbon <- rtruncnorm(sim_size, a=0, b=1, mean=.99, sd=.05)

hist(hydrocarbon)
# Reservoir
reservoir <- rtruncnorm(sim_size, a=0, b=1, mean=.8, sd=.1)

hist(reservoir)

# structure and seal
structure <- rep(1, sim_size)
seal <- rep(1, sim_size)

# probability of a producing well
prob_produce <- hydrocarbon * reservoir * structure * seal

hist(prob_produce)

# number of planned wells, comes from a uniform distribution
planned_wells <- rdunif(sim_size, 10, 30)
# hist(planned_wells)
barplot(table(rdunif(sim_size, 10, 30))) # sanity check

# tells us how many wells we will dig

# run a bernoulli to figure out the probability that the well is producing
# then randomly sample from the prob_producing of size of the planned_wells
# the probability is the respective probability

# randomly sample number of planned wells from the probability of producing wells

prod_wells <- data.frame(matrix(ncol = 3, nrow = 0))# create an empty list
colnames(prod_wells) <- c('total_wells', 'dry', 'producing')
for (i in planned_wells) { # iterate through the number of planned wells

  producing = sum(rbinom(i, 1, sample(prob_produce,i,replace=FALSE))) # could do with replacement
  dry = i -  sum(rbinom(i, 1, sample(prob_produce,i,replace=FALSE))) # but doesn't make sense if we will have large n
  prod_wells[nrow(prod_wells) + 1,] = c(i, dry, producing)
  # gives number of producing wells
}


# provide a histogram of the distribution of the proportion of wells that is producing

hist(prod_wells$dry)
hist(prod_wells$producing)

# Calculate the 5% VaR from this distribution.
# Calculate the 5% CVaR from this distribution.

# need to run the phase 2 simulations to get NPV of dry + wet
# idea is you need to run the phase two simulation for each well
