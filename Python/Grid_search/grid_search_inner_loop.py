#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun May 17 21:41:25 2020

@author: nitay
"""
import numpy as np
import pandas as pd
from Python.Grid_search.InnerCalibrationLoop import InnerCalibrationLoop
from Python.Grid_search.inner_loop import innerLoop

def GridSearchInnerLoop(environment_data,K,alpha,time_delta,partition_parameter):
    n = len(environment_data)
    T_final = T1 = np.zeros((n,2))
    inner_calibration_loop = InnerCalibrationLoop(environment_data, K, alpha,time_delta,partition_parameter)
    beta = inner_calibration_loop['beta']
    gamma = inner_calibration_loop['gamma']
    beta_hat = inner_calibration_loop['beta_hat']
    gamma_hat = inner_calibration_loop['gamma_hat']
    X = environment_data['X']
    Y = environment_data['Y']
    VW = environment_data['VW']
    Y_middle  = environment_data['Y_M']
    X_middle  = environment_data['X_M']
    VW = environment_data['VW'] 
    res = innerLoop(n, partition_parameter, beta, gamma,
                 time_delta ,alpha,
                 K,X[1], VW[1], Y[1])        
    x = res['x']
    vw = res['vw']
    y = res['y']
    for i in range(n):
        T1[i][0] = sum(x<=X[i])
        T1[i][1] = sum(vw<=VW[i])    
    T_final = T1 * time_delta
    TT = np.diff(T_final)-1
    COV = np.matmul(np.transpose(TT),TT)/(n-1)
    AD = beta * np.power(Y_middle,alpha) * np.maximum(0, 1 - X_middle / K)
    BD = gamma * Y_middle
    OBJ1=sum(np.log(pd.to_numeric(AD))+np.log(pd.to_numeric(BD))) #single figure - denom
    OBJ=OBJ1+(n/2)*np.log(np.linalg.det(COV)) # likelihood when we use two BM's    
    OBJB=sum(np.log(pd.to_numeric(AD)))+(n/2)*np.log(COV[0][0]) # BM for single component for X only
  
    return({'x' : x,
              'vw' : vw,
              'y' : y,
              'T1' : T1,
              'TT' : TT,
              'COV' : COV,
              'OBJ' : OBJ,
              'OBJB' : OBJB,
              'beta' : beta,
              'gamma' : gamma,
              'beta_hat' : beta_hat,
              'gamma_hat' : gamma_hat})