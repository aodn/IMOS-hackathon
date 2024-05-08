# Loading libraries
import xarray as xr
from glob import glob
import re
import os
import pandas as pd
import logging

# Defining functions
# Extracting bounding box from GPE3 necdf files
def bb_from_gpe3(filename, tag_id):
  '''
  Inputs:
    - filename - (string) Location of the GPE3 netcdf file
    - tag_id - (integer) Unique tag ID
  Outputs:
    - bb_df - (pandas DataFrame) Bounding box and tag ID
  '''
  
  #Open dataset
  ds = xr.open_dataset(filename)

  #Store in dictionary
  bb_dict = {'PTT': tag_id, \
            #Extracting bounding box (max and min coordinates)
            'min_lat': ds.coords['latitude'].min().values, \
            'max_lat': ds.coords['latitude'].max().values, \
            'min_lon': ds.coords['longitude'].min().values, \
            'max_lon': ds.coords['longitude'].max().values}

  #Turn to data frame
  bb_df = pd.DataFrame([bb_dict])
  
  #Return data frame
  return bb_df
