## Function to visualise TAT and TAD histograms
base_folder <- "2024/Projects/Fish-DAT"

#Load scripts
source(file.path(base_folder, "code/fishdat_functions.R"))

### ----------------------------------------------------------------------------------------------- ##
## test the function

folder_path <- file.path(base_folder, "data")

h <- histoviz(folder_path)

## summarise variables to larger time steps (daily) so it plots nicer
h_summary <-
  h %>% 
  mutate(date = date(date)) %>% 
  group_by(id, date, bin, variable) %>% 
  summarise(prop = mean(prop))

h_summary %>% 
  filter(variable %in% "depth") %>% 
  ggplot(aes(x = date, y = fct_rev(bin), fill = prop, id = variable)) +
  geom_tile() +
  facet_wrap(~id, ncol = 1, scales = "free") +
  scale_fill_viridis_c(direction = -1, na.value = NA, limits = c(0,100)) +
  labs(x = "Date", y = "Depth (m)", fill = "Proportion\nof time") +
  theme_bw()

h_summary %>% 
  filter(variable %in% "temp") %>% 
  ggplot(aes(x = date, y = bin, fill = prop, id = variable)) +
  geom_tile() +
  facet_wrap(~id, ncol = 1, scales = "free") +
  scale_fill_viridis_c(option = "A", direction = -1, na.value = NA, limits = c(0,100)) +
  labs(x = "Date", y = "Temperature (˚C)", fill = "Proportion\nof time") +
  theme_bw()





