################################################################################################
#
# APLICAÇÕES EM ESTATÍSTICA ESPACIAL - MBA Business Analytics e Big Data
#
# CASE - MAPA TEMÁTICO DE MUNICÍPIOS E POSTOS
#
################################################################################################


# PACOTES -----------------------------------------------------------------


# carrega as extensões SF, TMAP, DPLYR e PRYR
library(tmap)
library(sf)
library(dplyr)
library(pryr)


# loading other scripts do be used here ---------------------------------------
source("./src/util/auxiliary_functions.R")

# performing data (processed) loading -----------------------------------------
dataProcessedDirectory <- "./data/processed/"
shapefile_to_read <- paste(dataProcessedDirectory, 
                           "gas_prices_hist/gas_prices_hist.shp", 
                           sep = "")

# CARREGA MAPAS, DADOS E PREPARA O MAPA BASE ------------------------------

#Carrega o shapefile do mapa utilizando o pacote SF (Mais simples que o RGDAL)
mapa <- st_read(shapefile_to_read)

#O pacote SF carrega o shapefile como um dataframe (Diferente do RGDAL que carrega um SpatialPolygonsDataFrame)
class(mapa)

#Plota apenas os polígonos do mapa
plot(st_geometry(mapa))


# CRIANDO O DF DE LOCALIZAçÕES E TRANSFORMANDO EM SF ----------------------

csv_to_read <- paste(dataProcessedDirectory, 
                     "gas_prices_station.csv", 
                     sep = "")
gas_prices_station <- read.csv(csv_to_read)

#Pegar o CRS do mapa
st_crs(mapa)

#Transforma o DF de pontos em um formato para mapas adicionando o mesmo CRS do mapa
#Pode-se usar também  "crs = 4326" quando as coordenadas forem lat/long
localidades <- st_as_sf(gas_prices_station, coords = c("Lon", "Lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")

#Plota os pontos para testar
plot(st_geometry(localidades))


# RENDERIZA O MAPA --------------------------------------------------------

#Seta o mapa para o formato interativo (Com leaflet)
tmap_mode("view")

#Seta o título do gráfico
titulo = "Preço Médio de Revenda (R$)"

#Seta o os valores das quebras na legenda
breaks = c(3, 3.5, 4, 4.5, 5) 

#Primeira layer = Municípios
tm_shape(mapa) +
  
  #Utiliza as cores para montar o mapa temático baseado na coluna PcMedRev
  tm_fill(col = "PcMedRev", title = titulo, alpha = 0.8, palette = c("blue","white","red"), breaks = breaks) +
  
  #Insere bordas para facilitar a visualização das áreas
  tm_borders() +
  
  #Adiciona o estilo cobalt
  tm_style("cobalt") +
  
  #Insere a segunda layer = Localidades (lat,lon)
  tm_shape(localidades) + 
  
  #Desenha os pontos
  tm_dots(size = 0.05, col = "green", alpha =0.6)
