## ---- step_04_data_enhancement.R


# this step aims to improve the analysis by adding auxiliary information ------

# create_master_cidade_estado ----

master_cidade_estado <- tibble(estado = gas_prices_hist$estado,
                               cidade = gas_prices_hist$município)

master_cidade_estado <- distinct(master_cidade_estado)


# adding estado to gas_prices_station ----

gas_prices_station$estado <- plyr::mapvalues(gas_prices_station$cidade, 
                                             master_cidade_estado$cidade, 
                                             master_cidade_estado$estado, 
                                             warn_missing = FALSE)

# adding IBGE city code in gas_prices datasets ----

cities <- readOGR('data/raw/IBGE/br_municipios/BRMUE250GC_SIR.dbf', 
                  encoding = 'UTF-8', 
                  use_iconv = TRUE, 
                  stringsAsFactors = FALSE)

cities@data$NM_MUNICIP <- iconv(cities@data$NM_MUNICIP, to = 'ASCII//TRANSLIT')

UFs <- readOGR('data/raw/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.dbf', 
               encoding = 'UTF-8', 
               use_iconv = TRUE, 
               stringsAsFactors = FALSE)

UFs@data$NM_ESTADO    <- iconv(UFs@data$NM_ESTADO, to = 'ASCII//TRANSLIT')

cities@data$CD_GEOCUF  <- str_sub(cities@data$CD_GEOCMU, 1, 2)
cities@data$NM_ESTADO  <- plyr::mapvalues(cities@data$CD_GEOCUF, 
                                          UFs@data$CD_GEOCUF, 
                                          UFs@data$NM_ESTADO)

gas_prices_station$codigo.ibge <- plyr::mapvalues(paste(gas_prices_station$estado, 
                                                        gas_prices_station$cidade), 
                                                  paste(cities@data$NM_ESTADO, 
                                                        cities@data$NM_MUNICIP), 
                                                  cities@data$CD_GEOCMU, 
                                                  warn_missing = FALSE)

gas_prices_hist$codigo.ibge <- plyr::mapvalues(paste(gas_prices_hist$estado, gas_prices_hist$município), 
                                               paste(cities@data$NM_ESTADO, cities@data$NM_MUNICIP), 
                                               cities@data$CD_GEOCMU, warn_missing = FALSE)

# fix manual do codigo do IBGE (não encontrou no shapefile do IBGE por causa de caracter especial)
gas_prices_hist$codigo.ibge <- if_else(gas_prices_hist$codigo.ibge == 'RIO GRANDE DO SUL SANTANA DO LIVRAMENTO', 
                                         '4317103', 
                                         gas_prices_hist$codigo.ibge)


# adding geocoding using ggmap library ----

gas_prices_station$place <- paste(gas_prices_station$endereço, 
                                  gas_prices_station$bairro,
                                  gas_prices_station$cidade,
                                  gas_prices_station$estado,
                                  'BRAZIL', sep = ', ')
  
gas_prices_station <- mutate_geocode(gas_prices_station, place)

gas_prices_station <- filter(gas_prices_station, !(lon < -78))

# adding adding index to gas_prices dataset ----

gas_prices_station <- rowid_to_column(gas_prices_station, 'station.id')
gas_prices_hist <- rowid_to_column(gas_prices_hist, 'index')
