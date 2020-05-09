# loading required libraries --------------------------------------------------

# libraries for data prep
library(dplyr, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(tibble)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(lubridate, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)

# libraries for spatial data manipulation
library(ggmap)
library(rgdal, warn.conflicts = FALSE)

# set TRUE to recalculate the datasets, including the geocoding steps
# --------------------------------- WARNING -----------------------------------
# before you recalculate the dataset, make sure you have set up you google maps
# API Key in step_01_config_environment.R
data_prep_full <- FALSE

# load auxiliary functions ----------------------------------------------------
source("./src/util/auxiliary_functions.R")

# executing data preparation steps --------------------------------------------
source("./src/datapreparation/step_01_config_environment.R")

if (data_prep_full) {
  
  source("./src/datapreparation/step_02_data_ingestion.R")
  source("./src/datapreparation/step_03_data_cleaning.R")
  source("./src/datapreparation/step_04_data_enhancement.R")
  source("./src/datapreparation/step_05_data_save.R")

}

source("./src/datapreparation/step_06_data_load.R")
#source("./src/datapreparation/step_07_dataset_preparation.R")
