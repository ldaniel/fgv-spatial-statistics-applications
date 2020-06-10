## ---- step_02_data_ingestion.R

# performing data loading

# unzip required raw data sources ----

files_to_unzip <- c('data/raw/IBGE/br_municipios.zip', 
                    'data/raw/BIZROG_ZNMT2017_2019_Publ_10.4.zip')

for (file_to_unzip in files_to_unzip) {
  unzip(file_to_unzip, 
        exdir = tools::file_path_sans_ext(file_to_unzip), 
        overwrite = FALSE)
}

unzip('data/processed/gas_prices_hist/gas_prices_hist.zip', 
      exdir = 'data/processed/gas_prices_hist', overwrite = FALSE)

# import gas prices weekly historical ----

gas_prices_hist <- read_xlsx('data/raw/ANP_precos_gasolina/SEMANAL_MUNICIPIOS-2020.xlsx',
                             col_names = as.character(c(1:19)))

gas_prices_hist$`INTERVALO DE TEMPO` <- gas_prices_hist[[7, 1]]
gas_prices_hist$`INTERVALO DE TEMPO` <- 
  str_remove_all(gas_prices_hist$`INTERVALO DE TEMPO`, 'INTERVALO DE TEMPO: ')

gas_prices_hist$`PERÍODO` <- gas_prices_hist[[8, 1]]
gas_prices_hist$`PERÍODO` <- str_remove_all(gas_prices_hist$PERÍODO, 'PERÍODO: ')

gas_prices_hist$`COMBUSTÍVEL` <- gas_prices_hist[[9, 1]]
gas_prices_hist$`COMBUSTÍVEL` <- str_remove_all(gas_prices_hist$COMBUSTÍVEL, 'COMBUSTÍVEL: ')

gas_prices_hist$`TIPO RELATÓRIO` <- gas_prices_hist[[10, 1]]
gas_prices_hist$`TIPO RELATÓRIO` <- 
  str_remove_all(gas_prices_hist$`TIPO RELATÓRIO`, 'TIPO RELATÓRIO: ')

names(gas_prices_hist)[1:19] <- as.character(as.vector(gas_prices_hist[13, 1:19]))

gas_prices_hist <- gas_prices_hist[14:nrow(gas_prices_hist), ]

gas_prices_hist <- type_convert(gas_prices_hist,
                                col_types = cols(
                                  .default = col_character(),
                                  `DATA INICIAL` = col_integer(),
                                  `DATA FINAL` = col_integer(),
                                  `NÚMERO DE POSTOS PESQUISADOS` = col_integer(),
                                  `PREÇO MÉDIO REVENDA` = col_double(),
                                  `DESVIO PADRÃO REVENDA` = col_double(),
                                  `PREÇO MÍNIMO REVENDA` = col_double(),
                                  `PREÇO MÁXIMO REVENDA` = col_double(),
                                  `MARGEM MÉDIA REVENDA` = col_double(),
                                  `PREÇO MÉDIO DISTRIBUIÇÃO` = col_double(),
                                  `PREÇO MÉDIO DISTRIBUIÇÃO` = col_double(),
                                  `DESVIO PADRÃO DISTRIBUIÇÃO` = col_double(),
                                  `PREÇO MÍNIMO DISTRIBUIÇÃO` = col_double(),
                                  `PREÇO MÁXIMO DISTRIBUIÇÃO` = col_double(),
                                  `COEF DE VARIAÇÃO REVENDA` = col_double(),
                                  `COEF DE VARIAÇÃO DISTRIBUIÇÃO` = col_double()
                                ), 
                                na = '-')

gas_prices_hist$`DATA INICIAL` <- excel_numeric_to_date(gas_prices_hist$`DATA INICIAL`)
gas_prices_hist$`DATA FINAL` <- excel_numeric_to_date(gas_prices_hist$`DATA FINAL`)

names(gas_prices_hist) <- str_to_lower(make.names(names(gas_prices_hist)))

# import gas prices by gas station ----

files <- list.files('data/raw/ANP_precos_gasolina', full.names = TRUE)
files <- files[str_detect(files, 
                          'data/raw/ANP_precos_gasolina/RelatorioResumoPorMunicipioPosto*?')]

gas_prices_station <- tibble()

for (file in files) {

  df <- read_xlsx(file, col_names = as.character(c(1:9)))
  
  df$CIDADE <- df[[5, 1]]
  df$CIDADE <- str_remove_all(df$CIDADE, 'Síntese dos Preços Praticados - ')
  
  df$PRODUTO <- df[[6, 1]]
  df$PRODUTO <- str_match(df$PRODUTO, '- (.*?) R')[,2]
  
  df$`UNIDADE DE MEDIDA` <- df[[6, 1]]
  df$`UNIDADE DE MEDIDA` <- str_match(df$`UNIDADE DE MEDIDA`, 
                                                      paste('(?<= ' , 
                                                            df$PRODUTO[1], 
                                                            ' ).*(?! ', 
                                                            df$PRODUTO[1] , 
                                                            ' )', 
                                                            sep = ''))
  
  df$`DATA INICIAL` <- df[[7, 1]]
  df$`DATA INICIAL` <- str_match(df$`DATA INICIAL`, 'e (.*?) ')[,2]
  
  df$`DATA FINAL` <- df[[7, 1]]
  df$`DATA FINAL` <- str_match(df$`DATA FINAL`, '(?<= a ).*(?! a )')
  
  names(df)[1:9] <- as.character(as.vector(df[10, 1:9]))

  names(df)[8] <- 'fornecedor'
  
  df <- df[11:nrow(df), ]
  
  names(df) <- make.names(names(df))

  df <- filter(df, !is.na(DATA.COLETA))
  
  df <- type_convert(df,
                     col_types = cols(.default = col_character(),
                                      PREÇO.VENDA = col_double(),
                                      PREÇO.COMPRA = col_double(),
                                      DATA.COLETA = col_date(format = '%d/%m/%Y'),
                                      DATA.INICIAL = col_date(format = '%d/%m/%Y'),
                                      DATA.FINAL = col_date(format = '%d/%m/%Y')), 
                     na = '-')
  
  df$PREÇO.COMPRA <- df$PREÇO.COMPRA / 1000
  df$PREÇO.VENDA  <- df$PREÇO.VENDA / 1000

  gas_prices_station <- bind_rows(gas_prices_station, df)

}

names(gas_prices_station) <- str_to_lower(names(gas_prices_station))

names(gas_prices_station)[names(gas_prices_station) == 'modelidade.de.compra'] <- 
  'modalidade.de.compra'

rm(df, file, files)
invisible(gc())

# load mnemonic ----

gas_prices_hist_header <- read_xlsx('data/processed/mnemonico.xlsx', 
                                    sheet = 'gas_prices_hist')

gas_prices_station_header <- read_xlsx('data/processed/mnemonico.xlsx', 
                                       sheet = 'gas_prices_station')

PIB_header <- read_xlsx('data/processed/mnemonico.xlsx',
                        sheet = 'PIB')

POP_header <- read_xlsx('data/processed/mnemonico.xlsx',
                        sheet = 'Pop')

# import PIB data ----

pib <- read_xls('data/raw/IBGE/PIB dos Municípios - base de dados 2010-2017.xls', 
                col_names = PIB_header$mnemonico, 
                skip = 1)

# import POP data ----

pop <- read_xls('data/raw/IBGE/estimativa_TCU_2019_20200427.xls', 
                col_names = POP_header$mnemonico, 
                sheet = 'Municípios', 
                skip = 2)
