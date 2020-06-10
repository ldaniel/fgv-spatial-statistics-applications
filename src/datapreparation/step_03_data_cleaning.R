## ---- step_03_data_cleaning.R

# analysing missing values and other strange conditions -----------------------

# For this project, this step was not required.

# clean POP dataset ----

pop <- filter(pop, !is.na(pop$CdUF))

