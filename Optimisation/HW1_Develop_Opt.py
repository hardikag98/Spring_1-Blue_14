#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Feb  5 14:43:43 2023

@author: Jane
"""

# %% Import
import gurobipy as gp

from gurobipy import GRB

import numpy as np

import pandas as pd

import matplotlib.pyplot as plt

import math as math


# %% PERFORM OPTIMIZATION


# Read in the data
raw_stock=pd.read_csv('stocks_final.csv')

# Manipulate the data using pivot to get one row per date, the stock names as columns, and the %return values as data points

data_stock= raw_stock.pivot(index='Date', columns="Name", values="Percent Return")

stocks = data_stock.columns

# Get the number of stocks - we know this is 5
num_stocks=len(stocks)

# Get all of the expected returns - average return for each asset - vector of means
stock_return = data_stock.mean()

# Get covariance matrix - 5 x 5 matrix with variances down the diagonal (5 stocks)
cov_mat=data_stock.cov()

# Create an empty model
m = gp.Model('Bank_Stocks')

# Add a variable for each stock with an lb(lower bound) of zero - defaults to continuous
vars = pd.Series(m.addVars(stocks,lb=0), index=stocks)  

# update and keep these 5 continuous decision variables
m.update()
print(m)

# Calculate risk as variance - use matrix multiplication, multiplying the covariance matrix by the decision var series twice
portfolio_risk = cov_mat.dot(vars).dot(vars)

# Set objective - minimize the risk
m.setObjective(portfolio_risk, GRB.MINIMIZE)

# Set constraints

# #1 - sum of all decision variables, all percentages of stocks, needs to equal 1
m.addConstr(vars.sum() == 1, 'budget')

# #2 - return needs to be more than or equal to 0.005 
m.addConstr(stock_return.dot(vars) >=0.005,'return')

# update constraints
m.update()
print(m)

# lastly, optimize
m.optimize()

## Optimal objective 1.95140948e+00

print('Minimum Risk Portfolio:\n')

for v in vars:
    if v.x > 0:
        print('\t%s\t: %g' % (v.varname, v.x*100)) # prints it in percentage
#	C0 - ASC	:   19.9496
#	C1 - GLNCY: 31.2259
#	C2 - PDD	:   1.27598e-06 ~ essentially do not invest anything in PDD
#	C3 - ROST:  40.2282
#	C4 - TH:    8.59626


# print the minimized variance (lowest volatility of the portfolio on a daily basis) when the return is good enough (0.005 here)
print(m.objval)
# 3.72

# print return
print(stock_return.dot(m.x))

# print std
print(math.sqrt(m.objval))
# 1.93

# print proportion matrix
print(m.x)

# print in easier to see way
print("Proportion of RST (Ross Stores) Stock",round(m.x[3],3))
print("Proportion of PDD (Pinduoduo) Stock",round(m.x[2],3))
print("Proportion of TH (Target Hospitality) Stock",round(m.x[4],3))
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(m.x[0],3))
print("Proportion of GLNCY (Glencore Plc) Stock",round(m.x[1],3))


# you would need to compare this to other volitilities to see if it is really good or bad, but it is the best we can do under the constraints


# %% Efficient Frontier

# determine a set of portfolios that offer the best returns for a given risk, or the best risk for a given return, over all possible values

# Use same data_stock data frame

# calculate a new stock_volatility object that consists of the std of data_stock (before only calculated mean and cov)
stock_volatility = data_stock.std()

# print out the average returns for each stock to see the range of the means
print(stock_return)

# iterate over 0.03 -> 0.621 returns, since this is the range of the means - 500 values

returns = np.linspace( 0.046, 0.749, 500)

# initialize a list of returns (0.03 - 0.621 100 values), the lowest risk at each return, and the optimal proportions for the lowest risks
ret_list = []
risks = []
props = []

# loop through all 100 return values - saving risk and returns to plot
for ret in returns:
    m.reset(0)
    m = gp.Model("Portfolio_Optimization")
    m.setParam('OutputFlag', 0)
    vars=pd.Series(m.addVars(stocks,lb=0), index=stocks) 
    portfolio_risk = cov_mat.dot(vars).dot(vars)
    m.setObjective(portfolio_risk, GRB.MINIMIZE)
    m.addConstr(vars.sum() == 1, name = 'budget' )
    m.addConstr(stock_return.dot(vars) == ret , name = 'return_sim' ) # additional constraint so the return equals the return value specified
  
    m.optimize()  
    
    # add the std of risk to the risk list
    risks.append(np.sqrt(m.objval ) )
    
    # add the portfolio return to the return list
    ret_list.append(stock_return.dot(m.x) )
    
    # add the proportion matrix to a list
    props.append(m.x)

# plot
plt.rcParams.update({'font.size': 15})
plt.figure(figsize=(600/72,400/72)) # make the right size for the google doc
plt.plot( risks, returns ) 
plt.xlabel( 'Risk (standard deviation)' )
plt.ylabel( 'Return' )
plt.title( 'Portfolio Optimization Efficient Frontier' )
plt.locator_params(axis='both', nbins=10)
plt.plot()
plt.savefig("Efficient_Frontier.png", bbox_inches='tight')
plt.show()


# Look at what the return and proportions are at a specified risk

target = 2.5

min_difference = float('inf')
index = None
for i, num in enumerate(risks):
    diff = abs(num - target)
    if diff < min_difference:
        min_difference = diff
        index = i

risks_at_target = risks[index]
return_at_target = returns[index]
prop_at_target= props[index]

print("target=",target)
print("risk value closest to the target (std)=",round(risks_at_target,3))
print("return value at the specified risk",round(return_at_target,3))
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(prop_at_target[0],3))
print("Proportion of GLNCY (Glencore Plc) Stock",round(prop_at_target[1],3))
print("Proportion of PDD (Pinduoduo) Stock",round(prop_at_target[2],3))
print("Proportion of RST (Ross Stores) Stock",round(prop_at_target[3],3))
print("Proportion of TH (Target Hospitality) Stock",round(prop_at_target[4],3))
