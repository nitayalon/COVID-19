#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun May 17 22:06:53 2020

@author: nitay
"""

import numpy as np
import pandas as pd

def innerLoop(n, partition_parameter, beta ,gamma,
                 time_delta ,alpha,K,x_init, vw_init, y_init):
    m = partition_parameter * 5 * n;    
    x = np.repeat(x_init, partition_parameter)
    y = np.repeat(y_init, partition_parameter)
    vw = np.repeat(vw_init, partition_parameter)
    
    for i in np.arange(partition_parameter , m):
        max_val = np.maximum(0.0, 1 - x[i-1] / K)
        power_y = np.power(y[i-1],alpha)
        x = np.append(x, x[i-1] + beta * power_y * max_val * time_delta)
        vw = np.append(vw, vw[i-1] + gamma * y[i-1] * time_delta)
        y = np.append(y, np.maximum(0.0,x[i]-vw[i])) 
     
    results = pd.DataFrame({'x': x, 'y' : y, 'vw' : vw})
    return results
    