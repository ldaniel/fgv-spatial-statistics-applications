# loading required libraries --------------------------------------------------

# libraries for data prep
library(dplyr)
library(stringr)

# libraries for spatial data manipulation
library(spatialreg)
library(maps)
library(maptools)    
library(rgdal)     
library(sp)  
library(spdep)
library(bamlss)
library(gstat)
library(splancs)
library(spatstat)
library(pgirmess)
library(classInt)
library(spgwr)

# libraries for plots and visualization
library(RColorBrewer)
library(ggplot2)
library(ggExtra)

# libraries for modeling
library(MASS)

# loading other scripts do be used here ---------------------------------------
source("./src/util/auxiliary_functions.R")

# performing data (processed) loading -----------------------------------------
dataProcessedDirectory <- "./data/processed/"
shapefile_to_read <- paste(dataProcessedDirectory, 
                           "gas_prices_hist", 
                           sep = "")
target <- readOGR(shapefile_to_read, encoding="UTF-8")

# doing some spatial exploratory analysis -------------------------------------
View(target)
plot(target)
names(target)

# the variables ---------------------------------------------------------------
# "CodIBGE"    "Index"      "DtInicial"  "DtFinal"    "Regiao"     "Estado"    
# "Cidade"     "Produto"    "NmPostPesq" "UnidMedida" "PcMedRev"   "PcDevRev"  
# "PcMinRev"   "PcMaxRev"   "MgMedRev"   "CfVarRev"   "PcMedDist"  "PcDevDist"
# "PcMinDist"  "PcMaxDist"  "MgMedDist"  "CfVarDist"  "Periodo"    "Combust"
# "TipoRelat"  "DistMean"   "DistDev"    "DistMin"    "DistMax"    "RefinMean" 
# "RefinDev"   "RefinMin"   "RefinMax"   "PIB_2016"   "PIB_2017"   "PIBCap2016" 
# "PIBCap2017" "ChgPIB"     "ChgPIBCap"  "PopEst"     "X_COORD"    "Y_COORD"

# the quantitative variables --------------------------------------------------

# getting the class from every column in the target data
target.columns.class <- sapply(target@data, class)
View(target.columns.class)

# NmPostPesq	integer
# cMedRev	    numeric
# PcDevRev	  numeric
# PcMinRev	  numeric
# PcMaxRev	  numeric
# MgMedRev	  numeric
# CfVarRev	  numeric
# PcMedDist	  numeric
# PcDevDist	  numeric
# PcMinDist	  numeric
# PcMaxDist	  numeric
# MgMedDist	  numeric
# CfVarDist	  numeric
# DistMean	  numeric
# DistDev	    numeric
# DistMin	    numeric
# DistMax	    numeric
# RefinMean	  numeric
# RefinDev	  numeric
# RefinMin	  numeric
# RefinMax	  numeric
# PIB_2016	  numeric
# PIB_2017	  numeric
# PIBCap2016	numeric
# PIBCap2017	numeric
# ChgPIB	    numeric
# ChgPIBCap	  numeric
# PopEst	    numeric
# X_COORD	    numeric
# Y_COORD	    numeric

# variables analysis ----------------------------------------------------------
# Avaliando quais variáveis quantitativas apresentadas no shapefile apresenta 
# maior auto-correlação espacial. Descreva como implementou a matriz de 
# vizinhança. Apresentando o I de Moran e o mapa de auto-correlação espacial 
# local (LISA map) da variável escolhida.
# Obs: desconsidar as variáveis Codmuni, ID, X_coord e Y_coord nessa análise.

# getting the centroids of the polygons
xy <- coordinates(target) 

# neighborhood matrix from spatial polygons / adjacent polygons

# using the spdep library to generate first order
ap <- poly2nb(target, queen = T, row.names = target$Index)
lw <- nb2listw(ap, style = "W", zero.policy = TRUE)

class(ap)
summary(ap)
str(ap)

plot(target, col = 'cadetblue2', border = 'deepskyblue4', lwd = 1)
plot(ap, xy, col = 'red', lwd = 2, add = TRUE)

# using the bamlss library
nm <- neighbormatrix(target, type = "boundary", k = 3)


# implementing GWR ------------------------------------------------------------
# Implementando a regressão espacial GWR da variável y a partir de apenas 
# uma variável independente (não pode ser Index, X_coord nem Y_coord). 
# Apresentando o resultado da regressão linear simples e da regressão linear 
# espacial por GWR. Apresentando medidas da distribuição dos coeficientes 
# (min, Q1, Q2, Q3, máx), e da distribuição do R2 (min, Q1, Q2, Q3, máx) e 
# apresente os resultados globais da regressão (R2 global, basicamente).

# initial setup
res.palette <- colorRampPalette(c("red","orange","white","lightgreen","green"), 
                                space = "rgb")
pal <- res.palette(5)
par(mar = c(2, 0, 4, 0))

coords <- cbind(target$X_COORD, target$Y_COORD)

# GWR model (Geographically Weighted Regression)
target.gwr.sel <- gwr.sel(PcMedRev ~ 
                            DistMean +
                            DistDev +
                            DistMin +
                            RefinMean +
                            RefinDev +
                            RefinMin +
                            RefinMax +
                            ChgPIB +
                            ChgPIBCap, 
                          data = target, 
                          coords = coords, 
                          adapt = TRUE, 
                          method = "aic",
                          gweight = gwr.Gauss,
                          verbose = TRUE)

target.gwr.model <- gwr(PcMedRev ~ 
                          DistMean +
                          DistDev +
                          DistMin +
                          RefinMean +
                          RefinDev +
                          RefinMin +
                          RefinMax +
                          ChgPIB +
                          ChgPIBCap, 
                        data = target, 
                        coords = coords, 
                        bandwidth = target.gwr.sel,
                        gweight = gwr.Gauss,
                        adapt = target.gwr.sel,
                        hatmatrix = TRUE)
View(target.gwr.model)

# calculate global residual SST (SQT)
SST <- sum((target$DistMean - mean(target$DistMean)) ^ 2)
GWR_SSE <- target.gwr.model$results$rss
r2_GWR <- 1 - (GWR_SSE / SST)
r2_GWR

# residuals
target.gwr.residuals <- target.gwr.model$SDF$gwr.e

target.gwr.residuals.classes_fx <- classIntervals(target.gwr.residuals, n = 5, style = "fixed", 
                                                  fixedBreaks = c(-50,-25,-5,5,25,50), 
                                                  rtimes = 1)
cols.gwr.residuals <- findColours(target.gwr.residuals.classes_fx, pal)

plot(target, col = cols.gwr.residuals, main = "GWR Model (residuals)", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols.gwr.residuals,"palette"), bty = "n",
       legend = names(attr(cols.gwr.residuals, "table")), 
       title = "Residuals from GWR Model", ncol = 5)

moran.test(target.gwr.residuals, listw = lw, zero.policy = T)


# coefficients
target.gwr.coefficients <- target.gwr.model$SDF$DistMean

target.gwr.coefficients.classes_fx <- classIntervals(target.gwr.coefficients, n = 5, style = "fixed", 
                                                     fixedBreaks=c(-.005,-.003,-.001,.001,.003,.005), 
                                                     rtimes = 1)
cols.gwr.coefficients <- findColours(target.gwr.coefficients.classes_fx, pal)

plot(target, col = cols.gwr.coefficients, main = "GWR Model (coefficients)", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols.gwr.coefficients,"palette"), bty = "n",
       legend = names(attr(cols.gwr.coefficients, "table")),
       title = "Local Coefficient Estimates (PIBCap2016)", ncol = 3)

moran.test(target.gwr.coefficients, listw = lw, zero.policy = T)

# implementing stepwise -------------------------------------------------------
# Implementando um modelo de regressão linear multivariado stepwise da 
# variável y (significante a 5% ou 10%, utilize o que achar melhor). 
# Depois, “promovendo-a” a um modelo SAR. Apresentando os resultados 
# comparados (equação, R2).

# initial exploration in PcMedRev x DistMean
pcmedrev_by_DistMean_plot <- ggplot(data = target@data, 
                                    aes(x = target$DistMean,
                                        y = target$PcMedRev,
                                        color = target$DistMean)) +
  geom_point() +
  theme(legend.position = "none") +
  xlab("DistMean") +
  ylab("PcMedRev")

ggMarginal(pcmedrev_by_DistMean_plot, type = "histogram")

# runing the linear model multivaluated and looking at the residuals
target.ols.model <- lm(PcMedRev ~ 
                         DistMean +
                         DistDev +
                         DistMin +
                         RefinMean +
                         RefinDev +
                         RefinMin +
                         RefinMax +
                         ChgPIB +
                         ChgPIBCap, 
                       data = target)

summary(target.ols.model)

target$resid <- residuals(target.ols.model)
spplot(target, "resid", main = "Residuals")

# runing the lm multivaluated model
target.lm.multivaluated.model <- lm(PcMedRev ~ 
                                      DistMean +
                                      DistDev +
                                      DistMin +
                                      RefinMean +
                                      RefinDev +
                                      RefinMin +
                                      RefinMax +
                                      ChgPIB +
                                      ChgPIBCap, 
                                    data = target)

summary(target.lm.multivaluated.model)

# performing the stepwise selection
target.sar.model.stepwise <- step(target.lm.multivaluated.model, 
                                  direction = "both", 
                                  test = "F")

summary(target.sar.model.stepwise)

# runing the SAR model
target.lagsarlm.model <- lagsarlm(formula = PcMedRev ~ 
                                    DistMean +
                                    DistDev +
                                    DistMin +
                                    RefinMean +
                                    RefinDev +
                                    RefinMin +
                                    RefinMax +
                                    ChgPIB +
                                    ChgPIBCap, 
                                  data = target,
                                  listw = lw, 
                                  quiet = T,
                                  zero.policy = T, 
                                  tol.solve = 1e-12)

summary(target.lagsarlm.model)

# calculate global residual SST (SQT)
SST <- sum((target$PcMedRev - mean(target$PcMedRev)) ^ 2)
GWR_SSE <- target.lagsarlm.model$SSE
r2_GWR <- 1 - (GWR_SSE / SST)
r2_GWR

# maps
target$fitted_sem <- target.lagsarlm.model$fitted.values
spplot(target, "fitted_sem", main = "Fitted values")

target$actual_sem <- target.lagsarlm.model$y
spplot(target, "fitted_sem", main = "Actual values")

names(target.sar.model.stepwise$coefficients) <- 
  stringr::str_sub(names(target.sar.model.stepwise$coefficients), 1, 25)
summary(target.sar.model.stepwise)

# implementing GWR with stepwise ----------------------------------------------
# Promovendo o modelo final linear da Pergunta 6 a um modelo GWR. 
# Apresentando os resultados comparados (equação, R2).

# initial setup
res.palette <- colorRampPalette(c("red","orange","white","lightgreen","green"), 
                                space = "rgb")
pal <- res.palette(5)
par(mar = c(2, 0, 4, 0))

# GWR model (Geographically Weighted Regression)
target.gwr.multivaluated.sel <- gwr.sel(PcMedRev ~ 
                                          DistMean +
                                          DistDev +
                                          DistMin +
                                          RefinMean +
                                          RefinDev +
                                          RefinMin +
                                          RefinMax +
                                          ChgPIB +
                                          ChgPIBCap, 
                                        data = target, 
                                        coords = coords, 
                                        adapt = TRUE, 
                                        method = "aic",
                                        gweight = gwr.Gauss,
                                        verbose = TRUE)

target.gwr.multivaluated.model <- gwr(PcMedRev ~ 
                                        DistMean +
                                        DistDev +
                                        DistMin +
                                        RefinMean +
                                        RefinDev +
                                        RefinMin +
                                        RefinMax +
                                        ChgPIB +
                                        ChgPIBCap, 
                                      data = target, 
                                      coords = coords, 
                                      bandwidth = target.gwr.multivaluated.sel,
                                      gweight = gwr.Gauss,
                                      adapt = target.gwr.multivaluated.sel,
                                      hatmatrix = TRUE)

# calculate global residual SST (SQT)
SST <- sum((target$PcMedRev - mean(target$PcMedRev)) ^ 2)
GWR_SSE <- target.gwr.multivaluated.model$results$rss
r2_GWR <- 1 - (GWR_SSE / SST)
r2_GWR

# maps
target$fitted_sem <- target.gwr.multivaluated.model$lm$fitted.values
spplot(target, "fitted_sem", main = "Fitted values")

target$actual_sem <- target.gwr.multivaluated.model$lm$y
spplot(target, "fitted_sem", main = "Actual values")

# residuals
target.gwr.residuals <- target.gwr.model$SDF$gwr.e

target.gwr.residuals.classes_fx <- classIntervals(target.gwr.residuals, n = 5, style = "fixed", 
                                                  fixedBreaks = c(-50,-25,-5,5,25,50), 
                                                  rtimes = 1)
cols.gwr.residuals <- findColours(target.gwr.residuals.classes_fx, pal)

plot(target, col = cols.gwr.residuals, main = "GWR Model (residuals)", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols.gwr.residuals,"palette"), bty = "n",
       legend = names(attr(cols.gwr.residuals, "table")), 
       title = "Residuals from GWR Model", ncol = 5)

moran.test(target.gwr.residuals, listw = lw, zero.policy = T)

# coefficients
target.gwr.coefficients <- target.gwr.model$SDF$DistMean

target.gwr.coefficients.classes_fx <- classIntervals(target.gwr.coefficients, n = 5, style="fixed", 
                                                     fixedBreaks=c(-.005,-.003,-.001,.001,.003,.005), 
                                                     rtimes = 1)
cols.gwr.coefficients <- findColours(target.gwr.coefficients.classes_fx, pal)

plot(target, col = cols.gwr.coefficients, main = "GWR Model (coefficients)", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols.gwr.coefficients,"palette"), bty = "n",
       legend = names(attr(cols.gwr.coefficients, "table")),
       title = "Local Coefficient Estimates (urblevel)", ncol = 3)

moran.test(target.gwr.coefficients, listw = lw, zero.policy = T)