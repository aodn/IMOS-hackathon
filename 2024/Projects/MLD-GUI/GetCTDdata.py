# -*- coding: utf-8 -*-
"""
Created on Mon Apr  1 14:49:52 2024

@author: mphem
"""

# %% -----------------------------------------------------------------------------------------------
# Determine which computer this script is on

import os
if 'z3526971' in os.getcwd():
    account = 'C:\\Users\\z3526971\\'
else:
    account = 'C:\\Users\\mphem\\'

# %% Import packages

import xarray as xr
import os
import requests
import re
import numpy as np

os.chdir(account + r'\OneDrive - UNSW\Work\CTD_AGG')
import aggregated_profiles as agg
import PickleStuff as ps

# %% Get data

sites = ['NRSNSI', 'CH050', 'CH070', 'CH100', 'SYD100', 'SYD140', 'PH100', 'BMP070', 'BMP090', 'BMP120', 'NRSMAI', 'NRSKAI', 'NRSROT', 'NRSYON', 'WATR50']

CTDdata = {}
for s in sites:

    # get correct link for downloading data
    link = 0
    if 'NRS' in s:
        link = 'https://thredds.aodn.org.au/thredds/catalog/IMOS/ANMN/NRS/' + s + '/Biogeochem_profiles/catalog.html'
    if link == 0:
        link = 'https://thredds.aodn.org.au/thredds/catalog/IMOS/ANMN/NSW/' + s + '/Biogeochem_profiles/catalog.html'
    print(link)
    
    # get data
<<<<<<< Updated upstream
    CTDdata[s] = agg.AggregateProfiles(link,'TEMP')
        
=======
    CTDdata_TEMP[s] = agg.AggregateProfiles(link,'TEMP')
    
CTDdata_PSAL = {}
for s in sites:

    # get correct link for downloading data
    link = 0
    if 'NRS' in s:
        link = 'https://thredds.aodn.org.au/thredds/catalog/IMOS/ANMN/NRS/' + s + '/Biogeochem_profiles/catalog.html'
    if link == 0:
        link = 'https://thredds.aodn.org.au/thredds/catalog/IMOS/ANMN/NSW/' + s + '/Biogeochem_profiles/catalog.html'
    print(link)
    
    # get data
    CTDdata_PSAL[s] = agg.AggregateProfiles(link,'PSAL')
>>>>>>> Stashed changes
        
# %% save data as a pickle

ps.PickleSave(account + r'\OneDrive - UNSW\Work\CTD_AGG\CTDaggData.pickle',CTDdata)
