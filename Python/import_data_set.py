# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd

global_confirmed_cases = pd.read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
global_confirmed_deaths = pd.read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
global_confirmed_recovered =  pd.read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

def create_data_frame_for_state(state):
    confirmed_cases = global_confirmed_cases.drop(['Province/State', 'Lat', 'Long'], axis = 1)
    confirmed_cases = confirmed_cases[confirmed_cases['Country/Region'] == state]
    confirmed_deaths = global_confirmed_deaths.drop(['Province/State', 'Lat', 'Long'], axis = 1)
    confirmed_deaths  = confirmed_deaths [confirmed_deaths ['Country/Region'] == state]
    confirmed_recovered = global_confirmed_recovered.drop(['Province/State', 'Lat', 'Long'], axis = 1)
    confirmed_recovered  = confirmed_recovered[confirmed_recovered['Country/Region'] == state]
    
    confirmed_cases_df = pd.DataFrame(confirmed_cases.unstack()).reset_index(level=[0,1]) 
    confirmed_cases_df.columns = ['Date','Ah','Cases']
    confirmed_cases_df = confirmed_cases_df.set_index('Date').iloc[1:].drop('Ah',axis=1)

    confirmed_deaths_df = pd.DataFrame(confirmed_deaths.unstack()).reset_index(level=[0,1]) 
    confirmed_deaths_df.columns = ['Date','Ah','Deaths'] 
    confirmed_deaths_df = confirmed_deaths_df.set_index('Date').iloc[1:].drop('Ah',axis=1)
    
    confirmed_recovered_df = pd.DataFrame(confirmed_recovered.unstack()).reset_index(level=[0,1]) 
    confirmed_recovered_df.columns = ['Date','Ah','Recovered'] 
    confirmed_recovered_df = confirmed_recovered_df.set_index('Date').iloc[1:].drop('Ah',axis=1)
    
    frames = [confirmed_cases_df, confirmed_deaths_df, confirmed_recovered_df]
    nation_data = pd.concat(frames,axis=1, join='inner')    
    return(nation_data)

covid_data = create_data_frame_for_state('US')

def data_loader(covid_data, starting_day, cut_off_day= None):        
    if(cut_off_day is None):
        cutoff = covid_data.shape[0]
    else:
        cutoff = min([covid_data.shape[0], covid_data.shape[0] - cut_off_day])    
    X = covid_data['Cases'][starting_day:cutoff].reset_index()
    V = covid_data['Deaths'][starting_day:cutoff].reset_index()
    W = covid_data['Recovered'][starting_day:cutoff].reset_index()
    VW = V['Deaths'] + W['Recovered']
    Y = X['Cases'] - VW
    n = len(X)
    
    Y_middle = (Y[0:(n-1)].reset_index()+Y[1:n].reset_index()).iloc[:,1]
    Y_middle = Y_middle / 2
    Y_middle = Y_middle.append(Y.tail(1))
    
    X_middle = (X[0:(n-1)].reset_index()+X[1:n].reset_index())['Cases']
    X_middle = X_middle / 2
    X_middle = X_middle.append(X['Cases'].tail(1))
    
    data_for_analysis = pd.DataFrame({'X' : X['Cases'],
                                      'VW': VW, 
                                      'Y' : Y, 
                                      'X_M' : X_middle, 
                                      'Y_M' :Y_middle})
    return(data_for_analysis)