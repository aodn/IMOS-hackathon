## Function to extract error polygons from the .kmz file

## set how data are input into the function 
## (it only requires the path to the folder where -Histo files are found)
folder_path <- "~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data"

## parse all the histo files in the folder
csv_file <-
  list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
  map(function(x){list.files(x, pattern = "*GPE3.csv", full.names = T)}) %>% 
  unlist()

nc_files <-
  list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
  map(function(x){list.files(x, pattern = "*GPE3.nc", full.names = T)}) %>% 
  unlist()

library(tidync)

a <- 
  tidync(nc_files[1]) %>% 
  hyper_tibble() %>% 
  dplyr::select(longitude, latitude, twelve_hour_likelihoods, twelve_hour) %>% 
  pivot_wider(id_cols = c(1:2), names_from = "twelve_hour", values_from = "twelve_hour_likelihoods")

error_rast <- rast(a, type = "xyz")

plot(error_rast)

## input datasets
dat <- 
  read_csv("data/GPE3_output.csv", skip = 5) %>% 
  mutate(Date = dmy_hms(Date)) %>% 
  clean_names()

dat_sf <-
  dat %>% 
  st_as_sf(coords = c("most_likely_longitude", "most_likely_latitude"), crs = 4326, remove = F)

## extact error polygons and calculate lat and lon error
pols_05 <- extract_polys("data/GPE3_summary.kmz", prob_lim = 0.5) %>% st_make_valid() %>% rownames_to_column(var = "grouping")
pols_95 <- extract_polys("data/GPE3_summary.kmz", prob_lim = 0.95) %>% st_make_valid() %>% rownames_to_column(var = "grouping")
pols_99 <- extract_polys("data/GPE3_summary.kmz", prob_lim = 0.99) %>% st_make_valid() %>% rownames_to_column(var = "grouping")

## combine into single polygons
difference_list <- list()

for (i in 1:nrow(pols_05)) {
  diff1 <- pols_05[i,]
  diff2 <- st_difference(pols_95[i, ], pols_05[i, ]) %>% select(-ends_with(".1"))
  diff3 <- st_difference(pols_99[i, ], pols_95[i, ]) %>% select(-ends_with(".1"))
  difference_list[[i]] <- rbind(diff1, diff2, diff3)
}

pols <- 
  do.call("rbind", difference_list) %>% 
  mutate(probs = as_factor(prob_lim*100),
         date = datetime,
         grouping = fct_inseq(grouping))





















