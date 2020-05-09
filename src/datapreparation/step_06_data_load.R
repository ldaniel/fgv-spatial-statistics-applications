# save datasets ----
gas_prices_hist     <- read_rds('data/processed/gas_prices_hist.rds')
gas_prices_station  <- read_rds('data/processed/gas_prices_station.rds')

# load mnemônico ----

gas_prices_hist_header <- read_xlsx('data/processed/mnemonico.xlsx', sheet = 'gas_prices_hist')
gas_prices_station_header <- read_xlsx('data/processed/mnemonico.xlsx', sheet = 'gas_prices_station')


# load_shape_files ----

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
shp_gas_prices_station@data$DtColeta %<>% as_date(format = '%Y-%m-%d')
shp_gas_prices_station@data$DtFinal %<>% as_date(format = '%Y-%m-%d')
shp_gas_prices_station@data$DtInicial %<>% as_date(format = '%Y-%m-%d')

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
shp_gas_prices_hist@data$DtInicial %<>% as_date(format = '%Y-%m-%d')
shp_gas_prices_hist@data$DtFinal %<>% as_date(format = '%Y-%m-%d')

writeOGR(obj = shp_gas_prices_hist, dsn = 'data/processed/gas_prices_hist', layer = 'gas_prices_hist', driver = "ESRI Shapefile")
writeOGR(obj = shp_gas_prices_station, dsn = 'data/processed/gas_prices_station', layer = 'gas_prices_station', driver = "ESRI Shapefile")

