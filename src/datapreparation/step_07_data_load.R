## ---- step_07_data_load.R

# load datasets ----
gas_prices_hist           <- read_rds('./data/processed/gas_prices_hist.rds')
gas_prices_historical     <- read_rds('./data/processed/gas_prices_historical.rds')
gas_prices_station        <- read_rds('./data/processed/gas_prices_station.rds')

# load mnemônico ----

gas_prices_hist_header <- read_xlsx('./data/processed/mnemonico.xlsx', 
                                    sheet = 'gas_prices_hist')

gas_prices_station_header <- read_xlsx('./data/processed/mnemonico.xlsx', 
                                       sheet = 'gas_prices_station')


# load_shape_files ----

shp_gas_prices_station <- readOGR('./data/processed/gas_prices_station/gas_prices_station.dbf',
                                  encoding = 'UTF-8',
                                  use_iconv = TRUE,
                                  stringsAsFactors = FALSE)

shp_gas_prices_hist <- readOGR('./data/processed/gas_prices_hist/gas_prices_hist.dbf',
                               encoding = 'UTF-8', 
                               use_iconv = TRUE, 
                               stringsAsFactors = FALSE)
