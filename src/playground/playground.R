# loading required libraries ----------------------------------------------------------
# FGVR library for data science power-ups
library(fgvr)

# libraries for data prep
library(dplyr)
library(readr)
library(magrittr)
library(forcats)
library(lubridate)
library(stringr)
library(knitr)

#libraries for plots
library(ggplot2)
library(ggthemes)
library(ggpubr)

#libraries for spatial statistics
library(rgdal)     

# create CSV files to be read by Power BI -------------------------------------
# write.csv(gas_prices_hist,    file = "data/processed/gas_prices_hist.csv")
# write.csv(gas_prices_station, file = "data/processed/gas_prices_station.csv")

# first regression atempt ----

model <- lm(data = gas_prices_station, 
            formula = 'PcVenda ~ DistMean + DistDev + DistMin + DistMax + RefinMean + RefinDev + RefinMin  + RefinMax')

summary(model)

plot(model)

