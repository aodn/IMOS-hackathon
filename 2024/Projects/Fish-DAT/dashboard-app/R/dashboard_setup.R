
## SET YOUR RELATIVE REPO ROOT FOLDER (TO WHERE THIS FOLDER IS) HERE
## Get the relative path to the working directory folder
relative_base_folder <- ".."

# Load fish functions
functions_filepath <- list.files(relative_base_folder,
  pattern = "fishdat_functions.R",
  recursive = TRUE,
  full.names = TRUE
)

if(identical(functions_filepath, character(0)) ){
  stop("fishdat_functions.R not found. Please make sure the file is in the correct directory.")
}

source(functions_filepath)

# Load dashboard functions
dashboard_functions_filepath <- list.files(relative_base_folder,
  pattern = "dashboard_functions.R",
  recursive = TRUE,
  full.names = TRUE
)
source(dashboard_functions_filepath)

# Load metadata
metadata_filepath <- list.files(relative_base_folder,
  pattern = "HackathonMetadata.csv",
  recursive = TRUE,
  full.names = TRUE
)
metadata <- read_csv(metadata_filepath, show_col_types = FALSE)

# Load colour palette
palette_filepath <- list.files(relative_base_folder,
  pattern = "monthly_colour_palette.csv",
  recursive = TRUE,
  full.names = TRUE,
)

monthly_colour_palette <- read_csv(palette_filepath, show_col_types = FALSE)

# Write a message of what's variables are loaded
message("✨✨✨ Loaded into the environment: fishdat_functions.R, dashboard_functions.R, HackathonMetadata.csv as metadata, monthly_colour_palette.csv as monthly_colour_palette")

