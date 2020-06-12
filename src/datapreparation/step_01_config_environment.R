## ---- step_01_config_environment.R

# clearing everything before starting -----------------------------------------
# clear environment and memory
rm(list = ls()[ls() != 'data_prep_full'])
invisible(gc())

# clear console screen
cat("\014")

# clear plots
while (!is.null(dev.list()))  
  dev.off()

# setting the environment -----------------------------------------------------
options(encoding = "UTF-8")

info.username  <- Sys.info()[["user"]]
info.sysname   <- Sys.info()[["sysname"]]
info.machine   <- Sys.info()[["machine"]]
info.encoding  <- getOption("encoding")
directoryPath  <- dirname(rstudioapi::getSourceEditorContext()$path)
directoryPath  <- stringr::str_replace(directoryPath, "/src/datapreparation", "")

setwd(directoryPath)
getwd()


# setting up google maps API key for ggmaps -----------------------------------

# before you recalculate the dataset, make sure you have set up you google maps API below
register_google(key = "your-key-goes-here")

# to check current API key in use run:
# google_key()

# to supress API key in the console run:
# ggmap_hide_api_key()

# to show API key in the console run:
# ggmap_show_api_key()
