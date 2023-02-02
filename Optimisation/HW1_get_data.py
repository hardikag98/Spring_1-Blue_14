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

# set dates range 
start = datetime.datetime(2022,1,20) # January 20, 2022
end = datetime.datetime(2023,1,18) # January 18, 2023

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
# formula: ((open-close)/open)*100 = %return
# using close but not sure if I should use adj close?

stock_final['Percent Return'] = ((stock_final.Open-stock_final.Close)/stock_final.Open)*100

# save dataframs as csv to reduce pull request traffic
stock_final.to_csv("stocks_final.csv", index=True)
