# Install the wdpar package if it is not already installed
if (!requireNamespace("wdpar", quietly = TRUE)) {
    install.packages("wdpar", repos = "https://cran.rstudio.com/")
}

# load packages
library(wdpar)
library(dplyr)

# download protected area data for Malta
raw_pa_data <- wdpa_fetch("global",
    wait = TRUE
)

marine_raw_pa_data <- raw_pa_data %>% filter(MARINE == "2")

rm(raw_pa_data)

marine_pa_data <- wdpa_clean(marine_raw_pa_data)

# TODO: save the data to a file