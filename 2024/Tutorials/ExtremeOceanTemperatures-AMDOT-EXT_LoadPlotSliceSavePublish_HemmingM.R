
#==============================================================
# load packages

library(ncdf4)
library(ggplot2)

#==============================================================
# load data sets

URL <- 'https://thredds.aodn.org.au/thredds/dodsC/UNSW/NRS_extremes/Temperature_DataProducts/MAI090/MAI090_TEMP_EXTREMES_1944-2022_v1.nc'

# load file
R_MAI090 <- nc_open(URL)

nvar <- R_MAI090$nvar
varnames <- names(R_MAI090[['var']])
#==============================================================
# display variable names

variable_names <- names(R_MAI090$var)
print(variable_names)


# Get global attributes
global_attrs <- ncatt_get(R_MAI090, 0)

# Print global attributes
print(global_attrs)


#==============================================================
# get variables
temp_extreme_index <- ncvar_get(R_MAI090, "TEMP_EXTREME_INDEX")
temp <- ncvar_get(R_MAI090, "TEMP_INTERP")
temp_90 <- ncvar_get(R_MAI090, "TEMP_PER90")
mhw_event_duration <- ncvar_get(R_MAI090, "MHW_EVENT_DURATION")

# get TIME and convert to R time
TIME <- R_MAI090$dim$TIME[10]
TIME_R <- as.POSIXct((as.numeric(unlist(TIME)) - 7305)*86400, origin = "1970-01-01", tz = "UTC")

#==============================================================
# create dataframe containing variables at near-surface
df <- data.frame(TIME = TIME_R, TEMP_INTERP = temp[1,], TEMP_PER90 = temp_90[1,])
# Print the dataframe
summary(df)
str(df)
head(df, 10)

#==============================================================
# get MHW/HS data only
df <- data.frame(TIME = TIME_R, TEMP_INTERP = temp[1,], warmT = temp[1,], TEMP_PER90 = temp_90[1,])
df$warmT[(temp_extreme_index[1,] < 10)] <- NaN
head(df, 10)

#==============================================================
# Create plot
df$TIME <- as.Date(df$TIME)

ggplot(df, aes(x = TIME)) +
  geom_point(aes(y = TEMP_INTERP), color = "blue", fill = "blue", shape = 21, size = 2) +
  geom_line(data = df, aes(y = TEMP_PER90), color = "orange",linewidth=1.5) +
  geom_point(aes(y = warmT), color = "red", fill = "red", shape = 21, size = 2) +
  xlim(as.Date('2020-01-01'), as.Date('2022-01-01')) +
  labs(x = "TIME [deg C]", y = "Temperature [deg C]", title = "") +
  theme_minimal()

#==============================================================
# Find surface marine heatwaves
surf_mhws <- temp_extreme_index[1, ] == 12
# Extract surface durations
surf_duration <- mhw_event_duration[1, ]
# Find the longest duration
longest_duration <- max(surf_duration[surf_mhws], na.rm = TRUE)
# Identify the times with the longest duration
longest_selection <- surf_duration == longest_duration

# select data
longest_times <- TIME_R[which(longest_selection)]
longest_temp2m <- temp[1, which(longest_selection)]
longest_temp21m <- temp[2, which(longest_selection)]
longest_temp_90_2m <- temp_90[1, which(longest_selection)]
longest_temp_90_21m <- temp_90[2, which(longest_selection)]

# create dataframe
data_longest <- data.frame(TIME = longest_times,
                           TEMP_2m = longest_temp2m,
                           TEMP_21m = longest_temp21m,
                           PER90_2m = longest_temp_90_2m,
                           PER90_21m = longest_temp_90_21m)

#==============================================================
# export data as csv
# saving_path <- 'local\\path\\to\\save\\the\\CSV\\'
# write.csv(data_longest, paste(saving_path,'MAI_TEMP_PER90_LongestMHW.csv'))






