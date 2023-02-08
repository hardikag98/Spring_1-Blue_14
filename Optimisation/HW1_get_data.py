# -*- coding: utf-8 -*-
"""
Created on Thu Feb  2 13:58:58 2023

@author: Feliciti
"""
# code to pull financial data from 
# https://towardsdatascience.com/downloading-historical-stock-prices-in-python-93f85f059c1f

# Jane testing push/pull

# libraries
import pandas as pd
import yfinance as yf
import datetime

# set dates range 
start = datetime.datetime(2022,1,20) # January 20, 2022
end = datetime.datetime(2023,1,19) # January 19, 2023 - this way we have all data up until January 18 since end date is exclusive

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
            #calculate percent return
            per_return = ((current_close-prev_close)/prev_close)*100
            #save value in original dataframe
            stock_final.loc[((stock_final['Name'] == s) & (stock_final['Close'] == current_close)),'Percent Return'] = per_return
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
