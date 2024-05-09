# Loading libraries -------------------------------------------------------
library(tidyverse)

#Define directory containing data for all tags to be included
base_dir <- "2024/Projects/Fish-DAT"

#Loading metadata about individual tags ----------------------------------
tag_meta <- file.path(base_dir, "data/HackathonMetadata.csv") %>% 
  #Keep only relevant columns
  read_csv() %>%
  #Format dates correctly
  mutate(across(ends_with("Date"), 
                ~ parse_date_time(.x, orders = "%d/%m/%Y")))


# Getting list of netcdf files from GPE3 ----------------------------------
gpe3_files <- list.files(base_dir, "-GPE3.nc$", full.names = T, 
                         recursive = T) %>% 
  data.frame() %>% 
  rename("gpe3_path" = ".") %>% 
  mutate(PTT_ID = str_extract(dirname(gpe3_path), 
                              pattern = "data/(.*)", group = 1),
         PTT_ID = as.integer(PTT_ID)) %>% 
  right_join(tag_meta, join_by(PTT_ID)) %>% 
  relocate(gpe3_path, .after = Speed_Filter)


# Save metadata -----------------------------------------------------------
gpe3_files %>% 
  write_csv(file.path(base_dir, "data/HackathonMetadata.csv"))

