# %% PART 1 DATA PULL


# -*- coding: utf-8 -*-
"""
Created on Thu Feb  2 13:58:58 2023

@author: Feliciti
"""
# code to pull financial data from 
# https://towardsdatascience.com/downloading-historical-stock-prices-in-python-93f85f059c1f

# libraries
import pandas as pd
import yfinance as yf
import datetime
import gurobipy as gp
from gurobipy import GRB
import numpy as np
import matplotlib.pyplot as plt
import math as math

# set dates range 
start = datetime.datetime(2022,1,19) # January 20, 2022 <-need to have day prior to calculate first percent daily return
end = datetime.datetime(2023,1,19) # January 18, 2023 <-need extra day to be inclusive

# list stocks to pull data
# Ross Stores (RST), Pinduoduo (PDD), Target Hospitality (TH), 
# Ardmore Shipping Corp (ASC), Glencore Plc (GLNCY)

Symbols = ["ROST", "PDD", "TH", "ASC", "GLNCY"] # Ross Stores is actually ROST not RST
 
# create empty dataframe
stock_final = pd.DataFrame()
# iterate over each symbol
for i in Symbols:  
    
    # print the symbol which is being downloaded
    print( str(Symbols.index(i)) + str(' : ') + i, sep=',', end=',', flush=True)  
    
    try:
        # download the stock price 
        stock = []
        stock = yf.download(i,start=start, end=end, progress=False)
        
        # append the individual stock prices 
        if len(stock) == 0:
            None
        else:
            stock['Name']=i
            stock_final = stock_final.append(stock,sort=False)
    except Exception:
        None
        
# data manipulation
# create percent daily returns column
# formula: ((close - prior day close)/prior day close)*100=%dailyreturn

num_days=len(stock_final.loc[stock_final['Name']=='ROST'])

# for each stock
for s in Symbols:
    i = Symbols.index(s) #numeric index of stock name
    for n in range(num_days): #access each day individually
        current = n 
        prev_day = current - 1
        
        if prev_day >= 0:
            #get close values for today and yesterday
            current_close = stock_final.loc[stock_final.index[current]]['Close'][i]
            prev_close = stock_final.loc[stock_final.index[prev_day]]['Close'][i]
            
            current_high = stock_final.loc[stock_final.index[current]]['High'][i]
            current_low = stock_final.loc[stock_final.index[current]]['Low'][i]
            current_adjclose = stock_final.loc[stock_final.index[current]]['Adj Close'][i]
            current_vol = stock_final.loc[stock_final.index[current]]['Volume'][i]
            #calculate percent return
            per_return = ((current_close-prev_close)/prev_close)*100
            #save value in original dataframe
            #fix this to match to all columns 
            stock_final.loc[((stock_final['Name'] == s) & 
                             (stock_final['Close'] == current_close) &
                             (stock_final['High'] == current_high) &
                             (stock_final['Low'] == current_low) &
                             (stock_final['Adj Close'] == current_adjclose) &
                             (stock_final['Volume'] == current_vol)),'Percent Return'] = per_return
        else:
            #no return for the first day, these rows will be dropped later
            stock_final.loc[stock_final.index[n], 'Percent Return'] = 0
            n += 1 #step day forward
            continue

# Remove rows outside of date range
row_idx=[stock_final.index[0]]
stock_final = stock_final.drop(row_idx)

# save dataframs as csv to reduce pull request traffic
stock_final.to_csv("stocks_final.csv", index=True)


# %% PART 2 PERFORM OPTIMIZATION

"""
Created on Sun Feb  5 14:43:43 2023

@author: Jane
"""


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
print("Minimized Variance= ",round(m.objval,2))
# 4.01

# print return
print("Return for Minimized Variance= ",stock_return.dot(m.x))
# 0.27

# print std
print("Minimized STD= ",math.sqrt(m.objval))
# 2.00

# print in easier to see way
print("Proportion of RST (Ross Stores) Stock",round(m.x[3],3)*100)
print("Proportion of PDD (Pinduoduo) Stock",round(m.x[2],3)*100)
print("Proportion of TH (Target Hospitality) Stock",round(m.x[4],3)*100)
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(m.x[0],3)*100)
print("Proportion of GLNCY (Glencore Plc) Stock",round(m.x[1],3)*100)

# make a pie chart of these proportions for the bluf

prop= [m.x[0]*100, m.x[1]*100,m.x[3]*100,m.x[4]*100]
labels= ["Ardmore Shipping Corp", "Glencore Plc",  "Ross Stores", "Target Hospitality"]

plt.rcParams.update({'font.size': 15})
plt.figure(figsize=(600/72,400/72)) # make the right size for the google doc
prop, labels = zip(*sorted(zip(prop, labels), reverse=False))
plt.pie(prop, labels=labels, startangle=90, autopct='%1.1f%%')
plt.title("Proportions of Stocks In Minimized Risk Portfolio", y=1.1)
plt.axis('equal')
plt.savefig("Pie_Chart.png", bbox_inches='tight')
plt.show()



# you would need to compare this to other volitilities to see if it is really good or bad, but it is the best we can do under the constraints


# %% Efficient Frontier

# determine a set of portfolios that offer the best returns for a given risk, or the best risk for a given return, over all possible values

# Use same data_stock data frame

# calculate a new stock_volatility object that consists of the std of data_stock (before only calculated mean and cov)
stock_volatility = data_stock.std()

# print out the average returns for each stock to see the range of the means
print(stock_return)

# iterate over 0.03 -> 0.621 returns, since this is the range of the means - 500 values

returns = np.linspace( 0.10923, 0.79882, 500)

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
plt.plot( risks, returns*100 , color='black', linewidth=2, zorder=2 ) 
plt.xlabel( 'Risk (std)' )
plt.ylabel( 'Return (%)' )
plt.title( 'Portfolio Optimization Efficient Frontier' )
plt.locator_params(axis='both', nbins=10)

# Adding call out symbols
p1 = 2
min_difference = float('inf')
p1_index = None
for i, num in enumerate(risks):
    diff = abs(num - p1)
    if diff < min_difference:
        min_difference = diff
        p1_index = i
p2 = 2.5

min_difference = float('inf')
p2_index = None
for i, num in enumerate(risks):
    diff = abs(num - p2)
    if diff < min_difference:
        min_difference = diff
        p2_index = i
p3 = 5.19

min_difference = float('inf')
p3_index = None
for i, num in enumerate(risks):
    diff = abs(num - p3)
    if diff < min_difference:
        min_difference = diff
        p3_index = i
        
x= [risks[p1_index],risks[p2_index],risks[p3_index]]
y= [returns[p1_index]*100,returns[p2_index]*100,returns[p3_index]*100]
markers = ['o', '*', 's']
colors = ['blue', 'blue', 'blue']

for i in range(3):
    plt.scatter(x[i], y[i], s=300, c=colors[i], marker=markers[i], zorder=3)

plt.plot()
plt.savefig("Efficient_Frontier.png", bbox_inches='tight')
plt.show()


# Look at what the return and proportions are at a specified risk

target = 2

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

print("Proportion of RST (Ross Stores) Stock",round(prop_at_target[3],3)*100)
print("Proportion of PDD (Pinduoduo) Stock",round(prop_at_target[2],3)*100)
print("Proportion of TH (Target Hospitality) Stock",round(prop_at_target[4],3)*100)
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(prop_at_target[0],3)*100)
print("Proportion of GLNCY (Glencore Plc) Stock",round(prop_at_target[1],3)*100)

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

print("Proportion of RST (Ross Stores) Stock",round(prop_at_target[3],3)*100)
print("Proportion of PDD (Pinduoduo) Stock",round(prop_at_target[2],3)*100)
print("Proportion of TH (Target Hospitality) Stock",round(prop_at_target[4],3)*100)
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(prop_at_target[0],3)*100)
print("Proportion of GLNCY (Glencore Plc) Stock",round(prop_at_target[1],3)*100)


target = 3.0

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

print("Proportion of RST (Ross Stores) Stock",round(prop_at_target[3],3)*100)
print("Proportion of PDD (Pinduoduo) Stock",round(prop_at_target[2],3)*100)
print("Proportion of TH (Target Hospitality) Stock",round(prop_at_target[4],3)*100)
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(prop_at_target[0],3)*100)
print("Proportion of GLNCY (Glencore Plc) Stock",round(prop_at_target[1],3)*100)


target = 3.5

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

print("Proportion of RST (Ross Stores) Stock",round(prop_at_target[3],3)*100)
print("Proportion of PDD (Pinduoduo) Stock",round(prop_at_target[2],3)*100)
print("Proportion of TH (Target Hospitality) Stock",round(prop_at_target[4],3)*100)
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(prop_at_target[0],3)*100)
print("Proportion of GLNCY (Glencore Plc) Stock",round(prop_at_target[1],3)*100)


target = 4

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

print("Proportion of RST (Ross Stores) Stock",round(prop_at_target[3],3)*100)
print("Proportion of PDD (Pinduoduo) Stock",round(prop_at_target[2],3)*100)
print("Proportion of TH (Target Hospitality) Stock",round(prop_at_target[4],3)*100)
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(prop_at_target[0],3)*100)
print("Proportion of GLNCY (Glencore Plc) Stock",round(prop_at_target[1],3)*100)


target = 4.5

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

print("Proportion of RST (Ross Stores) Stock",round(prop_at_target[3],3)*100)
print("Proportion of PDD (Pinduoduo) Stock",round(prop_at_target[2],3)*100)
print("Proportion of TH (Target Hospitality) Stock",round(prop_at_target[4],3)*100)
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(prop_at_target[0],3)*100)
print("Proportion of GLNCY (Glencore Plc) Stock",round(prop_at_target[1],3)*100)


target = 5

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

print("Proportion of RST (Ross Stores) Stock",round(prop_at_target[3],3)*100)
print("Proportion of PDD (Pinduoduo) Stock",round(prop_at_target[2],3)*100)
print("Proportion of TH (Target Hospitality) Stock",round(prop_at_target[4],3)*100)
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(prop_at_target[0],3)*100)
print("Proportion of GLNCY (Glencore Plc) Stock",round(prop_at_target[1],3)*100)

target = 5.19

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

print("Proportion of RST (Ross Stores) Stock",round(prop_at_target[3],3)*100)
print("Proportion of PDD (Pinduoduo) Stock",round(prop_at_target[2],3)*100)
print("Proportion of TH (Target Hospitality) Stock",round(prop_at_target[4],3)*100)
print("Proportion of ASC (Ardmore Shipping Corp) Stock",round(prop_at_target[0],3)*100)
print("Proportion of GLNCY (Glencore Plc) Stock",round(prop_at_target[1],3)*100)


## Additional notes not in paper

#Portfolio to Double Return - Lower Risk
#looks like a risk of just 0.5 more (2.5) about doubles the return (from 27% to 54%) - breakdown looks like   - 100.0% increase
#Medium Risk Portfolio Options
#from 2.5 - 3 increases the return by around 12% points (66%) - 22.2% increase in return with 0.5 increase in risk
#from 3 - 3.5 increases the return by around 6% points (72%) - 9.1% increase in return with 0.5 increase in risk
#from 3.5 - 4 increases the return by around 3% points (75%) - 4.2% increase in return with 0.5 increase in risk
#from 3.5 - 4.5 increases the return by around 2% points (77%) - 2.7% increase in return with 0.5 increase in risk
#from 4.5 - 5  increases the return by around 2% points (79%) - 2.5% increase in return with 0.5 increase in risk
#We could graph the change in %increase? this could go in the appendix? not sure, just putting the info here for now
#Portfolio to Triple Return - High Risk 
#Highest risk profile - all of money in Target Hospitality for a return of 80% and a risk of 5.19
#triples the return from minimum risk (27% * 3 = 81%) - so slightly under  - but with way more risk
#196.3% increase from minimum risk
