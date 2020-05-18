#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun May 17 20:04:54 2020

@author: nitay
"""
import numpy as np
from Python.import_data_set import data_loader
from Python.Grid_search.grid_search_inner_loop import GridSearchInnerLoop

def gridSearch(national_data, starting_day, cut_off_day = None, prediction_period = None,
                         environment_parameter_list = None):
    partition_parameter = 100
    time_delta = 1/partition_parameter # dt
    hhh_upper_limit = 400
    environment_data = data_loader(national_data,starting_day, cut_off_day)    
    NNN = np.floor(max(environment_data['X']) / 100) #What is the grid of K
    hhh = np.arange(1, hhh_upper_limit, 1)
    alpha_grid = np.linspace(0.4,0.6,num = 100)
    k_grid = np.max(environment_data['X']) + hhh * NNN
    grid_parameters = np.transpose([np.tile(k_grid, len(alpha_grid)), np.repeat(alpha_grid, len(k_grid))])
    OBJB = OBJ = COV = results = list()
    for i in np.arange(0,len(grid_parameters)):
        grid_search_inner_loop_results = GridSearchInnerLoop(environment_data,
                                                             grid_parameters[i][0],
                                                    grid_parameters[i][1],
                                                    time_delta,
                                                    partition_parameter)
        results.append(grid_search_inner_loop_results)
        OBJB.append(grid_search_inner_loop_results['OBJB'])
        OBJ.append(grid_search_inner_loop_results['OBJ'])
        COV.append(grid_search_inner_loop_results['COV'])        
        if i % 500 == 0:
            print(f'Iteration number {i}')
    return({'grid_search_results':grid_search_inner_loop_results,
            'cov_mat' : COV,
            'objb_llk ': OBJB,
            'obj_llk' : OBJ
            })
    