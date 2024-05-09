###########################################################
# Downloading fishing effort data from Global Fishing Watch
# Author: Denisse Fierro Arcos
# Date: 2024-05-08
# This script uses Python functions included in the 
# supporting_functions.py script.
# **Note that before running this script, you will need to
# follow the set up instructions in the README file inside
# this folder**
###########################################################

# Loading libraries -------------------------------------------------------
library(reticulate)
library(tidyverse)

#Define directory containing data for all tags to be included
base_dir <- "2024/Projects/Fish-DAT"
#Load Python script
source_python(file.path(base_dir, "code/supporting_functions.py"))


#Loading metadata about individual tags ----------------------------------
tag_meta <- file.path(base_dir, "data/HackathonMetadata.csv") %>% 
  #Keep only relevant columns
  read_csv() 


# Downloading fishing pressure data ---------------------------------------
#Start empty data frame to store results 
fish_effort <- data.frame()

#Loop through each row
for(i in 1:nrow(tag_meta)){
  df <- download_gfw_data(tag_meta$gpe3_path[i],
                          tag_id = tag_meta$PTT_ID[i], 
                          token = Sys.getenv("GFW_token"),
                          date_start = tag_meta$Deployment_Date[i], 
                          date_end = tag_meta$Detachment_Date[i]) 
  #Store everything in a single data frame
  fish_effort <- fish_effort %>% 
    bind_rows(df)
}


# Save dataframe ----------------------------------------------------------
fish_effort %>% 
  write_csv(file.path(base_dir, "data/fishing_pressure_GFW.csv"))

