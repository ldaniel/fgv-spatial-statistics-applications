## ---- step_06_data_save.R

# this step aims to save the prepared dataset ------

# delete intermediate datasets ----
rm(cities, master_cidade_estado, UFs)
invisible(gc)

# save datasets ----
write_rds(as_tibble(shp_gas_prices_hist@data), 'data/processed/gas_prices_hist.rds')
write_rds(as_tibble(shp_gas_prices_station@data), 'data/processed/gas_prices_station.rds')
write_rds(gas_prices_hist, 'data/processed/gas_prices_historical.rds')
write_rds(PIB_change, 'data/processed/pib_change.rds')

write_csv(as_tibble(shp_gas_prices_hist@data), 'data/processed/gas_prices_hist.csv')
write_csv(as_tibble(shp_gas_prices_station@data), 'data/processed/gas_prices_station.csv')
write_csv(gas_prices_hist, 'data/processed/gas_prices_historical.csv')
write_csv(PIB_change, 'data/processed/pib_change.csv')

writeOGR(obj = shp_gas_prices_hist, 
         dsn = 'data/processed/gas_prices_hist', 
         layer = 'gas_prices_hist', 
         driver = "ESRI Shapefile", 
         overwrite_layer = TRUE, 
         encoding = 'UTF-8')

writeOGR(obj = shp_gas_prices_station, 
         dsn = 'data/processed/gas_prices_station',
         layer = 'gas_prices_station', 
         driver = "ESRI Shapefile", 
         overwrite_layer = TRUE,
         encoding = 'UTF-8')
