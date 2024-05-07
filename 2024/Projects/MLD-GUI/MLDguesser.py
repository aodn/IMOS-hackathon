
# %% -------------------------------------------------------------
##################################################################
# Import Modules

# account = 'mphem'
account = 'z3526971'

# define paths
class options:
    core_path = ('C:\\Users\\' + account + '\\OneDrive - UNSW\\Work\\Trends\\' + 
             'Scripts\\Core_scripts\\')
    utilities_path = ('C:\\Users\\' + account + '\\OneDrive - UNSW\\Work\\Trends\\' +
                  'Scripts\\Utilities\\')
    plotting_path = ('C:\\Users\\' + account + '\\OneDrive - UNSW\\Work\\' +
                     'Trends\\Output\\Plots\\')
    raw_data_path = ('C:\\Users\\' + account + '\\OneDrive - UNSW\\Work\\' +
                     'Trends\\Data\\Raw_data\\')
    processed_data_path = ('C:\\Users\\' + account + '\\OneDrive - UNSW\\Work\\' +
                     'Trends\\Data\\Processed_data\\')
    
# ---------------------------
# general functions
import os
import numpy as np
from scipy.io import savemat
import pandas as pd
# autocorrelation
import statsmodels.api as sm
# ---------------------------
# Import functions from script
os.chdir(options.core_path)
import Trends_functions as TF

import xarray as xr
import matplotlib.pyplot as plt

import scipy.stats as ss
import random

# %% -------------------------------------------------------------
##################################################################
# Estimate the MLD by eye

##############
# MAI
##############

MLDs_chosen = []
MLDs_chosen_t = []
profs = random.sample(range(0, len(MAI_un_dates)), 100)
for n in range(len(profs)):
    c = MAI_CTD_t == MAI_un_dates[n]

    plt.figure(figsize=(5,7))
    ax = plt.gca()
    ax.set_title('click on MLD', picker=True)
    ax.set_ylabel('Depth [m]', picker=True)
    line, = ax.plot(MAI_CTD_T[c],MAI_CTD_D[c], picker=5,linewidth=3)
    plt.xlim([11, 19])
    plt.gca().invert_yaxis()
    MLDs_chosen.append(plt.ginput(1)) # temp, depth
    MLDs_chosen_t.append(MAI_un_dates[n])
    plt.close()
    
MLDs_chosen = np.squeeze(np.array(MLDs_chosen))
MLDs_chosen_T = MLDs_chosen[:,0]  
MLDs_chosen_MLD = MLDs_chosen[:,1]  

data_dict = {'MLD':MLDs_chosen_MLD,
              'TIME': np.array(MLDs_chosen_t),
              'MLD_TEMP': MLDs_chosen_T}
chosen = xr.Dataset(data_dict)
chosen.to_netcdf(options.processed_data_path + 'MAI_MLD_eyeballed_100profs.nc')

MAI_eye = xr.open_dataset(options.processed_data_path + 'MAI_MLD_eyeballed_100profs.nc')






