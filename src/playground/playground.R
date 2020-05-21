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
library(forcats)

#libraries for plots
library(ggplot2)
library(ggthemes)
library(ggpubr)

#libraries for spatial statistics
library(rgdal)     

# first regression atempt ----

source("./src/datapreparation/step_07_data_load.R")

# gas_prices_station$PcVenda %<>% scale()
# gas_prices_station$DistMean %<>% scale()
# gas_prices_station$DistDev %<>% scale()
# gas_prices_station$DistMin %<>% scale()
# gas_prices_station$DistMax %<>% scale()
# gas_prices_station$RefinMean %<>% scale()
# gas_prices_station$RefinDev %<>% scale()
# gas_prices_station$RefinMin %<>% scale()
# gas_prices_station$RefinMax %<>% scale()


gas_prices_station$DistMean %<>% divide_by(1000)
gas_prices_station$DistDev %<>% divide_by(1000)
gas_prices_station$DistMin %<>% divide_by(1000)
gas_prices_station$DistMax %<>% divide_by(1000)
gas_prices_station$RefinMean %<>% divide_by(1000)
gas_prices_station$RefinDev %<>% divide_by(1000)
gas_prices_station$RefinMin %<>% divide_by(1000)
gas_prices_station$RefinMax %<>% divide_by(1000)

gas_prices_hist <- filter(gas_prices_hist, !is.na(PcMedRev))

gas_prices_hist$DistDev %<>% divide_by(1000)
gas_prices_hist$DistMin %<>% divide_by(1000)
gas_prices_hist$DistMax %<>% divide_by(1000)
gas_prices_hist$RefinMean %<>% divide_by(1000)
gas_prices_hist$RefinDev %<>% divide_by(1000)
gas_prices_hist$RefinMin %<>% divide_by(1000)
gas_prices_hist$RefinMax %<>% divide_by(1000)

model_station <- lm(data = gas_prices_station,
                    formula = 'PcVenda ~ DistMean + DistDev + DistMin + DistMax + RefinMean + RefinDev + RefinMin  + RefinMax')

model_city <- lm(data = gas_prices_hist,
                    formula = 'PcMedRev ~ DistMean + DistDev + DistMin + DistMax + RefinMean + RefinDev + RefinMin  + RefinMax')

summary(model_station)
summary(model_city)

# preços de gasolina por cidade
ggplot(data = gas_prices_station) +
  geom_boxplot(aes(y = PcVenda, 
                   x = fct_reorder(Cidade, PcVenda, .desc = FALSE),
                   fill = Estado), 
               show.legend = FALSE
               ) +
  coord_flip()

# preços de gasolina vs DistMIn
ggplot(data = gas_prices_station, aes(x = DistMin, y = PcVenda)) +
  geom_point(aes(color = PcVenda), position = 'jitter') +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = FALSE, color = 'black') +
  scale_x_log10()




