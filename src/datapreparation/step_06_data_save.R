# this step aims to save the prepared dataset ------

# delete intermediate datasets ----
rm(cities, master_cidade_estado, UFs)
invisible(gc)

# save datasets ----
write_rds(as_tibble(shp_gas_prices_hist@data), 'data/processed/gas_prices_hist.rds')
write_rds(as_tibble(shp_gas_prices_station@data), 'data/processed/gas_prices_station.rds')

write_csv(as_tibble(shp_gas_prices_hist@data), 'data/processed/gas_prices_hist.csv')
write_csv(as_tibble(shp_gas_prices_station@data), 'data/processed/gas_prices_station.csv')

writeOGR(obj = shp_gas_prices_hist, dsn = 'data/processed/gas_prices_hist', layer = 'gas_prices_hist', driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(obj = shp_gas_prices_station, dsn = 'data/processed/gas_prices_station', layer = 'gas_prices_station', driver = "ESRI Shapefile", overwrite_layer = TRUE)
