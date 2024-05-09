# -*- coding: utf-8 -*-
"""
Created on Mon Apr  1 14:49:52 2024

@author: mphem
"""

# %% -----------------------------------------------------------------------------------------------
# Determine which computer this script is on
# This code is for Michael's directories, adapt for yours

import os
if 'z3526971' in os.getcwd():
    account = 'C:\\Users\\z3526971\\'
else:
    account = 'C:\\Users\\mphem\\'

# %% Import packages

import os
os.chdir(r'C:\Users\mphem\OneDrive - UNSW\Work\QAQC_NRT_AODNhackathon_2024\AODNhackathon\aodn-hackathon\2024\Projects\MLD-GUI')

import aggregated_profiles as agg # import code to aggregate CTD profiles
import PickleStuff as ps # functions to save/load pickle files

# %% Get data

# sites = ['NRSNSI', 'CH050', 'CH070', 'CH100', 'SYD100', 'SYD140', 'PH100', 'BMP070', 'BMP090', 'BMP120', 'NRSMAI', 'NRSKAI', 'NRSROT', 'NRSYON', 'WATR50']
sites = ['PH100']

CTDdata_TEMP = {}
for s in sites:

    # get correct link for downloading data
    link = 0
    if 'NRS' in s:
        link = 'https://thredds.aodn.org.au/thredds/catalog/IMOS/ANMN/NRS/' + s + '/Biogeochem_profiles/catalog.html'
    if link == 0:
        link = 'https://thredds.aodn.org.au/thredds/catalog/IMOS/ANMN/NSW/' + s + '/Biogeochem_profiles/catalog.html'
    print(link)
    # get data
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
    
CTDdata_DENS = {}
for s in sites:

    # get correct link for downloading data
    link = 0
    if 'NRS' in s:
        link = 'https://thredds.aodn.org.au/thredds/catalog/IMOS/ANMN/NRS/' + s + '/Biogeochem_profiles/catalog.html'
    if link == 0:
        link = 'https://thredds.aodn.org.au/thredds/catalog/IMOS/ANMN/NSW/' + s + '/Biogeochem_profiles/catalog.html'
    print(link)
    
    # get data
    CTDdata_DENS[s] = agg.AggregateProfiles(link,'DENS')

# %% save data as a pickle

ps.PickleSave('Data\\PH100CTD_TEMP.pickle', CTDdata_TEMP)
ps.PickleSave('Data\\PH100CTD_PSAL.pickle', CTDdata_PSAL)
ps.PickleSave('Data\\PH100CTD_DENS.pickle', CTDdata_DENS)

