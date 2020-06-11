## ---- step_05_dataset_preparation.R

# dataset preparation ---------------------------------------------------------

# fix shape files created in QGIS ----

shp_gas_prices_station <- readOGR('data/processed/gas_prices_station/gas_prices_station.dbf',
                                  encoding = 'UTF-8',
                                  use_iconv = TRUE,
                                  stringsAsFactors = FALSE)

shp_gas_prices_hist <- readOGR('data/processed/gas_prices_hist/gas_prices_hist.dbf',
                               encoding = 'UTF-8', 
                               use_iconv = TRUE, 
                               stringsAsFactors = FALSE)

names(shp_gas_prices_hist@data) <- gas_prices_hist_header$mnemonico
names(shp_gas_prices_station@data) <- gas_prices_station_header$mnemonico
 
shp_gas_prices_station@data$StationID %<>% as.integer()
shp_gas_prices_station@data$PcVenda %<>% as.double()
shp_gas_prices_station@data$PcCompra %<>% as.double()
shp_gas_prices_station@data$Lon %<>% as.double()
shp_gas_prices_station@data$Lat %<>% as.double()
shp_gas_prices_station@data$DtColeta %<>% as_date(format = '%Y-%m-%d', tz = "America/Sao_Paulo")
shp_gas_prices_station@data$DtFinal %<>% as_date(format = '%Y-%m-%d', tz = "America/Sao_Paulo")
shp_gas_prices_station@data$DtInicial %<>% as_date(format = '%Y-%m-%d', tz = "America/Sao_Paulo")
 
shp_gas_prices_hist@data$Index %<>% as.integer()
shp_gas_prices_hist@data$NmPostPesq %<>% as.integer()
shp_gas_prices_hist@data$PcMedRev %<>% as.double()
shp_gas_prices_hist@data$PcDevRev %<>% as.double()
shp_gas_prices_hist@data$PcMinRev %<>% as.double()
shp_gas_prices_hist@data$PcMaxRev %<>% as.double()
shp_gas_prices_hist@data$PcMedDist %<>% as.double()
shp_gas_prices_hist@data$PcDevDist %<>% as.double()
shp_gas_prices_hist@data$PcMinDist %<>% as.double()
shp_gas_prices_hist@data$PcMaxDist %<>% as.double()
shp_gas_prices_hist@data$MgMedRev %<>% as.double()
shp_gas_prices_hist@data$MgMedDist %<>% as.double()
shp_gas_prices_hist@data$CfVarRev %<>% as.double()
shp_gas_prices_hist@data$CfVarDist %<>% as.double()
shp_gas_prices_hist@data$DtInicial %<>% as_date(format = '%Y-%m-%d', tz = "America/Sao_Paulo")
shp_gas_prices_hist@data$DtFinal %<>% as_date(format = '%Y-%m-%d', tz = "America/Sao_Paulo")

# prep gas_prices_historical ----

names(gas_prices_hist) <- plyr::mapvalues(names(gas_prices_hist), 
                                          gas_prices_hist_header$descricao, 
                                          gas_prices_hist_header$mnemonico, 
                                          warn_missing = FALSE)

# Adding PIB and POP Data

PIB_change <- select(pib, CodIBGE, Ano, PIBCorr, PIBPerCapCorr) %>%
  filter(Ano >= 2016) %>% 
  pivot_wider(values_fn = sum, names_from = Ano, values_from = c(PIBCorr, PIBPerCapCorr)) %>% 
  mutate(ChgPIB = PIBCorr_2017 / PIBCorr_2016 - 1,
         ChgPIBCap = PIBPerCapCorr_2017 / PIBPerCapCorr_2016 - 1)

PIB_change <-
  left_join(shp_gas_prices_hist@data, PIB_change, by = 'CodIBGE') %>%
  left_join(pop, by = 'CodIBGE') %>% 
  select(c(CodIBGE, PIBCorr_2016, PIBCorr_2017, PIBPerCapCorr_2016, PIBPerCapCorr_2017, ChgPIB, ChgPIBCap, PopEstimada)) %>% as_tibble()

names(PIB_change) <- c("CodIBGE", "PIB_2016", "PIB_2017", "PIBCap2016", "PIBCap2017", "ChgPIB", "ChgPIBCap", "PopEst")

PIB_change$CodIBGE <- as.integer(PIB_change$CodIBGE)
