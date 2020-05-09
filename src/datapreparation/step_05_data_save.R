# this step aims to save the prepared dataset ------

# delete intermediate datasets ----
rm(cities, master_cidade_estado, UFs)
invisible(gc)

# save datasets ----
write_rds(gas_prices_hist, 'data/processed/gas_prices_hist.rds')
write_rds(gas_prices_station, 'data/processed/gas_prices_station.rds')

write_csv(gas_prices_hist, 'data/processed/gas_prices_hist.csv')
write_csv(gas_prices_station, 'data/processed/gas_prices_station.csv')