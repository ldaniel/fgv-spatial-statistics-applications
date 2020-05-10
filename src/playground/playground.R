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
library(feather)
library(fastDummies)
library(reshape2)
library(knitr)

#libraries for plots
library(ggplot2)
library(ggthemes)
library(ggcorrplot)
library(ggpubr)
library(plotly)

# libraries for data clean
library(VIM)
library(rms)
library(mctest)

# libraries for modeling
library(caret)
library(gmodels)
library(MASS)
library(rpart)
library(rpart.plot)
library(adabag)
library(randomForest)

# libraries for spatial data manipulation
library(spatialreg)
library(maps)
library(maptools)    
library(rgdal)     
library(sp)  
library(spdep)
library(bamlss)
library(gstat)
library(splancs)
library(spatstat)
library(pgirmess)
library(classInt)
library(spgwr)

# libraries for measures
library(hmeasure)
library(pROC)

# create CSV files to be read by Power BI -------------------------------------
# write.csv(gas_prices_hist,    file = "data/processed/gas_prices_hist.csv")
# write.csv(gas_prices_station, file = "data/processed/gas_prices_station.csv")

# first regression atempt ----
model <- lm(data = gas_prices_station, 
            formula = 'PcVenda ~ DistMean + DistDev + DistMin + DistMax + RefinMean + RefinDev + RefinMin  + RefinMax')

summary(model)

plot(model)

