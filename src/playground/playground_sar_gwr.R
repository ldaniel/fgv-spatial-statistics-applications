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
library(tmap)
library(ggplot2)
library(ggExtra)

# libraries for modeling
library(MASS)

# loading other scripts do be used here ---------------------------------------
source("./src/util/auxiliary_functions.R")

# performing data (processed) loading -----------------------------------------
dataProcessedDirectory <- "./data/processed/"
shapefile_to_read <- paste(dataProcessedDirectory, "distancias_municipios_distribuidoras", sep = "")
target <- readOGR(shapefile_to_read, encoding="UTF-8")

# doing some spatial exploratory analysis -------------------------------------
View(target)
plot(target)
names(target)

# the variables ---------------------------------------------------------------
# NAME
# GEOCODIGO_
# NOME_MUNIC
# UF 
# HubName
# HubDist 

# Pergunta 1 ------------------------------------------------------------------
# Qual das variáveis quantitativas apresentadas no shapefile crime_mg apresenta 
# maior auto-correlação espacial? Descreva como implementou a matriz de 
# vizinhança. Apresente o I de Moran e o mapa de auto-correlação espacial local 
# (LISA map) da variável escolhida e também de pelo menos outras 3 variáveis.
# Obs: desconsidere as variáveis Codmuni, ID, X_coord e Y_coord nessa análise.

# getting the centroids of the polygons
xy <- coordinates(target) 

# neighborhood matrix from spatial polygons / adjacent polygons

# using the spdep library to generate first order
ap <- poly2nb(target, queen = T, row.names = target$ID)
lw <- nb2listw(ap, style = "W", zero.policy = TRUE)

class(ap)
summary(ap)
str(ap)

plot(target, col = 'cadetblue2', border = 'deepskyblue4', lwd = 1)
plot(ap, xy, col = 'red', lwd = 2, add = TRUE)

# using the bamlss library
nm <- neighbormatrix(target, type = "boundary")
print(nm)
plotneighbors(target)
plotneighbors(target, type = "delaunay")
plotneighbors(target, type = "dist", d1 = 0, d2 = 0.15)

# global autocorrelation tests: Moran's I
moran.test.AREA     <- moran.test(target$AREA, listw = lw, zero.policy = T) 
moran.test.INDICE94 <- moran.test(target$INDICE94, listw = lw, zero.policy = T)
moran.test.INDICE95 <- moran.test(target$INDICE95, listw = lw, zero.policy = T)
moran.test.GINI_91  <- moran.test(target$GINI_91, listw = lw, zero.policy = T)
moran.test.POP_94   <- moran.test(target$POP_94, listw = lw, zero.policy = T)
moran.test.POP_RUR  <- moran.test(target$POP_RUR, listw = lw, zero.policy = T)
moran.test.POP_URB  <- moran.test(target$POP_URB, listw = lw, zero.policy = T)
moran.test.POP_FEM  <- moran.test(target$POP_FEM, listw = lw, zero.policy = T)
moran.test.POP_MAS  <- moran.test(target$POP_MAS, listw = lw, zero.policy = T)
moran.test.POP_TOT  <- moran.test(target$POP_TOT, listw = lw, zero.policy = T)
moran.test.URBLEVEL <- moran.test(target$URBLEVEL, listw = lw, zero.policy = T)
moran.test.PIB_PC   <- moran.test(target$PIB_PC, listw = lw, zero.policy = T)

moran.test.all <- rbind(t(data.frame("AREA" = moran.test.AREA$estimate)),
                        t(data.frame("INDICE94" = moran.test.INDICE94$estimate)),
                        t(data.frame("INDICE95" = moran.test.INDICE95$estimate)),
                        t(data.frame("GINI_91" = moran.test.GINI_91$estimate)),
                        t(data.frame("POP_94" = moran.test.POP_94$estimate)),
                        t(data.frame("POP_RUR" = moran.test.POP_RUR$estimate)),
                        t(data.frame("POP_URB" = moran.test.POP_URB$estimate)),
                        t(data.frame("POP_FEM" = moran.test.POP_FEM$estimate)),
                        t(data.frame("POP_MAS" = moran.test.POP_MAS$estimate)),
                        t(data.frame("POP_TOT" = moran.test.POP_TOT$estimate)),
                        t(data.frame("URBLEVEL" = moran.test.URBLEVEL$estimate)),
                        t(data.frame("PIB_PC" = moran.test.PIB_PC$estimate)))

moran.test.all <- as_tibble(moran.test.all, rownames = "Variables")
moran.test.all %>% arrange(desc(`Moran I statistic`))

print(moran.test.all)

# Moran scatterplot for INDICE94
par(mar = c(4,4,1.5,0.5))
moran.plot(target$INDICE94, 
           listw = lw, 
           zero.policy = T,
           pch = 16, 
           col = "black",
           cex = .5, 
           quiet = F,
           labels = as.character(target$MUNIC),
           xlab = "Percent for INDICE94",
           ylab = "Percent for INDICE94 (Spatial Lag)", 
           main = "Moran Scatterplot")

# LISA map for INDICE94 
locm <- localmoran(target$INDICE94,lw)

target$sPPOV <- scale(target$INDICE94)
target$lag_sPPOV <- lag.listw(lw, target$sPPOV)

plot(x = target$sPPOV, y = target$lag_sPPOV, main = "Moran Scatterplot PPOV")
abline(h = 0, v = 0)
abline(lm(target$lag_sPPOV ~ target$sPPOV), lty = 3, lwd = 4, col = "red")

# check out the outliers click on one or two and then hit escape or click finish
identify(target$sPPOV, target$lag_sPPOV, target$INDICE94, cex = 0.8)

target$quad_sig <- NA
target@data[(target$sPPOV >= 0 & target$lag_sPPOV >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
target@data[(target$sPPOV <= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
target@data[(target$sPPOV <= 0 & target$lag_sPPOV >= 0) & (locm[, 5] > 0.05), "quad_sig"] <- 5 

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(target$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
par(mar = c(4,0,4,1))
plot(target, col = colors[np])
mtext("Local Moran's I - INDICE94", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")

# LISA map for INDICE95 
locm <- localmoran(target$INDICE95,lw)

target$sPPOV <- scale(target$INDICE95)
target$lag_sPPOV <- lag.listw(lw, target$sPPOV)
target$quad_sig <- NA
target@data[(target$sPPOV >= 0 & target$lag_sPPOV >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
target@data[(target$sPPOV <= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
target@data[(target$sPPOV <= 0 & target$lag_sPPOV >= 0) & (locm[, 5] > 0.05), "quad_sig"] <- 5 

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(target$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
par(mar = c(4,0,4,1))
plot(target, col = colors[np])
mtext("Local Moran's I - INDICE95", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")

# LISA map for URBLEVEL 
locm <- localmoran(target$URBLEVEL,lw)

target$sPPOV <- scale(target$URBLEVEL)
target$lag_sPPOV <- lag.listw(lw, target$sPPOV)
target$quad_sig <- NA
target@data[(target$sPPOV >= 0 & target$lag_sPPOV >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
target@data[(target$sPPOV <= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
target@data[(target$sPPOV <= 0 & target$lag_sPPOV >= 0) & (locm[, 5] > 0.05), "quad_sig"] <- 5 

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(target$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
par(mar = c(4,0,4,1))
plot(target, col = colors[np])
mtext("Local Moran's I - URBLEVEL", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")

# LISA map for POPRUR 
locm <- localmoran(target$POP_RUR,lw)

target$sPPOV <- scale(target$POP_RUR)
target$lag_sPPOV <- lag.listw(lw, target$sPPOV)
target$quad_sig <- NA
target@data[(target$sPPOV >= 0 & target$lag_sPPOV >= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 1
target@data[(target$sPPOV <= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 2
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 3
target@data[(target$sPPOV >= 0 & target$lag_sPPOV <= 0) & (locm[, 5] <= 0.05), "quad_sig"] <- 4
target@data[(target$sPPOV <= 0 & target$lag_sPPOV >= 0) & (locm[, 5] > 0.05), "quad_sig"] <- 5 

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(target$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
par(mar = c(4,0,4,1))
plot(target, col = colors[np])
mtext("Local Moran's I - POPRUR", cex = 1.5, side = 3, line = 1)
legend("topleft", legend = labels, fill = colors, bty = "n")

# Pergunta 2 ------------------------------------------------------------------
# Implemente o modelo espacial auto-regressivo (SAR) da variável Indice95 
# (índice de criminalidade em 1995 de Minas Gerais) a partir de apenas uma 
# variável independente (não pode ser Indice94, Codmuni, ID, X_coord nem 
# Y_coord). Apresente o resultado da regressão linear simples e da regressão 
# linear espacial. Apresente as equações e interprete seus coeficientes.

# initial setup
res.palette <- colorRampPalette(c("red","orange","white","lightgreen","green"), 
                                space = "rgb")
pal <- res.palette(5)
par(mar = c(2, 0, 4, 0))

# linear regresion model
target.lm.model <- lm(INDICE95 ~ URBLEVEL, data = target)
summary(target.lm.model)

target.lm.model.residuals <- target.lm.model$residuals

target.lm.model.class_fx <- classIntervals(target.lm.model.residuals, 
                                           n = 5,
                                           style = "fixed",
                                           fixedBreaks = c(-50,-25,-5,5,25,50),
                                           rtimes = 1)

cols.lm <- findColours(target.lm.model.class_fx, pal)

plot(target, col = cols.lm, main = "OLS Model", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols.lm, "palette"), bty = "n",
       legend = names(attr(cols.lm, "table")), title = "Residuals from OLS Model",
       ncol = 5)

moran.test(target.lm.model.residuals, listw = lw, zero.policy = T)

# SAR model (Spatial Auto-Regressive)
target.sar.model <- lagsarlm(INDICE95 ~ URBLEVEL, 
                             data = target, 
                             listw = lw,
                             zero.policy = T, 
                             tol.solve = 1e-12)
summary(target.sar.model)
target.sar.model$rest.se
target.sar.model$residuals

target.sar.model.residuals <- target.sar.model$residuals

target.sar.model.class_fx <- classIntervals(target.sar.model.residuals, 
                                            n = 5, 
                                            style = "fixed",
                                            fixedBreaks = c(-50,-25,-5,5,25,50),
                                            rtimes = 1)

cols.sar <- findColours(target.sar.model.class_fx, pal)

plot(target, col = cols.sar, main = "SAR Model", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols.sar, "palette"), bty = "n",
       legend = names(attr(cols.sar, "table")), title = "Residuals from SAR Model",
       ncol = 5)

moran.test(target.sar.model.residuals, listw = lw, zero.policy = T)

# Pergunta 3 ------------------------------------------------------------------
# Para essa variável que você escolheu, o modelo espacial SAR apresentou ganhos 
# significantes com relação ao modelo linear simples? Justifique sua resposta.
# Obs: Sugere-se fazer essa atividade no GeoDA ou no R.

# Pergunta 4 ------------------------------------------------------------------
# Implemente a regressão espacial GWR da variável Indice95 (índice de 
# criminalidade em 1995 de Minas Gerais) a partir de apenas uma variável 
# independente (não pode ser Indice94, Codmuni, ID, X_coord nem Y_coord). 
# Apresente o resultado da regressão linear simples e da regressão linear 
# espacial por GWR. Apresente medidas da distribuição dos coeficientes 
# (min, Q1, Q2, Q3, máx), e da distribuição do R2 (min, Q1, Q2, Q3, máx) e 
# apresente os resultados globais da regressão (R2 global, basicamente).
# Obs: Sugere-se fazer essa atividade no ArcGIS ou no R.

# initial setup
res.palette <- colorRampPalette(c("red","orange","white","lightgreen","green"), 
                                space = "rgb")
pal <- res.palette(5)
par(mar = c(2, 0, 4, 0))

coords <- cbind(target$X_COORD, target$Y_COORD)

# GWR model (Geographically Weighted Regression)
target.gwr.sel <- gwr.sel(INDICE95 ~ URBLEVEL, 
                          data = target, 
                          coords = coords, 
                          adapt = TRUE, 
                          method = "aic",
                          gweight = gwr.Gauss,
                          verbose = TRUE)

target.gwr.model <- gwr(INDICE95 ~ URBLEVEL, 
                        data = target, 
                        coords = coords, 
                        bandwidth = target.gwr.sel,
                        gweight = gwr.Gauss,
                        adapt = target.gwr.sel,
                        hatmatrix = TRUE)
View(target.gwr.model)

# calculate global residual SST (SQT)
SST <- sum((target$INDICE95 - mean(target$INDICE95)) ^ 2)
GWR_SSE <- target.gwr.model$results$rss
r2_GWR <- 1 - (GWR_SSE / SST)
r2_GWR

# # calculate local R-Squared 
# kGauss <- round(target.gwr.sel * length(target[,1]))
# myknn <- knearneigh(coords, k = kGauss, longlat = FALSE, RANN = FALSE)
# mynb <- knn2nb(myknn, sym = TRUE)
# ap <- target@data
# 
# for (i in 1:length(target[,1]))
# {
#   # seleciona e ordena os índices dos polígonos vizinhos de i
#   vizinhos_i <- sort(c(i, mynb[[i]]))
#   
#   # seleciona o slot "polygons" (lista de pol?gonos) dos vizinhos de i
#   listapoly_vizinhos_i <- slot(target, "polygons")
#   poligonos_i <- subset(listapoly_vizinhos_i, ap[,1] %in% vizinhos_i)
#   
#   # converte apenas i e seus vizinhos para o objeto SpatialPolygons
#   sp_i <- SpatialPolygons(poligonos_i)
#   
#   # cria os objetos nb e listwi apenas para i e seus vizinhos
#   mynb_i <- poly2nb(sp_i)
#   mylistw_i <- nb2listw(mynb_i, style="W", zero.policy=TRUE)
#   
#   ap_i <- ap[vizinhos_i,]
#   
#   # calculate global residual SS of local sample
#   sum_square_glo_i <- sum((ap_i$INDICE95 - mean(ap_i$INDICE95)) ^ 2)
#   
#   lm.ap_i <- lm(INDICE95 ~ URBLEVEL, data = ap_i)
#   
#   residuos[i] <- lm.ap_i$residuals[ap_i$ID==i]
#   previstos[i] <- lm.ap_i$fitted.values[ap_i$ID==i]
#   ParW[i] <- lm.ap_i$rho
#   ParIntercepto[i] <- lm.ap_i$coefficients[1]
#   ParEnergia[i] <- lm.ap_i$coefficients[2]
#   r2_local[i] <- 1 - (lm.ap_i$SSE/sum_square_glo_i)
#   print(i)
# }

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
target.gwr.coefficients <- target.gwr.model$SDF$URBLEVEL

target.gwr.coefficients.classes_fx <- classIntervals(target.gwr.coefficients, n = 5, style = "fixed", 
                                                     fixedBreaks=c(-.005,-.003,-.001,.001,.003,.005), 
                                                     rtimes = 1)
cols.gwr.coefficients <- findColours(target.gwr.coefficients.classes_fx, pal)

plot(target, col = cols.gwr.coefficients, main = "GWR Model (coefficients)", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols.gwr.coefficients,"palette"), bty = "n",
       legend = names(attr(cols.gwr.coefficients, "table")),
       title = "Local Coefficient Estimates (URBLEVEL)", ncol = 3)

moran.test(target.gwr.coefficients, listw = lw, zero.policy = T)

# Pergunta 5 ------------------------------------------------------------------
# Para essa variável que você escolheu, o modelo espacial GWR apresentou ganhos 
# significantes com relação ao modelo linear simples? Justifique sua resposta.

# Pergunta 6 ------------------------------------------------------------------
# Implemente um modelo de regressão linear multivariado stepwise da variável 
# Indice95 (significante a 5% ou 10%, utilize o que achar melhor). Depois, 
# “promova-o” a um modelo SAR. Apresente os resultados comparados (equação, 
# R2). Qual modelo você escolheria como final? Se desejar, apresente mapas 
# que sustente sua justificativa.

# initial exploration in INDICE95 x AREA
indice95_by_urblevel_plot <- ggplot(data = target@data, 
                                    aes(x = target$INDICE95, 
                                        y = target$URBLEVEL, 
                                        color = target$URBLEVEL)) +
  geom_point() +
  theme(legend.position = "none") +
  xlab("INDICE95") +
  ylab("AREA")

ggMarginal(indice95_by_urblevel_plot, type = "histogram")

# runing the linear model multivaluated and looking at the residuals
target.ols.model <- lm(INDICE95 ~ 
                         AREA + 
                         INDICE94 + 
                         GINI_91 +
                         POP_94 +
                         POP_RUR +
                         POP_URB +
                         POP_FEM +
                         POP_MAS +
                         POP_TOT +
                         URBLEVEL +
                         PIB_PC, 
                       data = target)

summary(target.ols.model)

target$resid <- residuals(target.ols.model)
spplot(target, "resid", main = "Residuals")

# runing the lm multivaluated model
target.lm.multivaluated.model <- lm(formula =INDICE95 ~ 
                                      AREA + 
                                      INDICE94 + 
                                      GINI_91 +
                                      POP_94 +
                                      POP_RUR +
                                      POP_URB +
                                      POP_FEM +
                                      POP_MAS +
                                      POP_TOT +
                                      URBLEVEL +
                                      PIB_PC, 
                                    data = target)

summary(target.lm.multivaluated.model)

# performing the stepwise selection
target.sar.model.stepwise <- step(target.lm.multivaluated.model, 
                                  direction = "both", 
                                  test = "F")

summary(target.sar.model.stepwise)

# runing the SAR model
target.lagsarlm.model <- lagsarlm(formula =INDICE95 ~ 
                                    AREA + 
                                    INDICE94 + 
                                    GINI_91 +
                                    POP_94 +
                                    POP_RUR +
                                    POP_URB +
                                    POP_FEM +
                                    POP_MAS +
                                    POP_TOT +
                                    URBLEVEL +
                                    PIB_PC, 
                                  data = target,
                                  listw = lw, 
                                  quiet = T)
summary(target.lagsarlm.model)

# calculate global residual SST (SQT)
SST <- sum((target$INDICE95 - mean(target$INDICE95)) ^ 2)
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

# Pergunta 7 (bônus) ----------------------------------------------------------
# Promova o modelo final linear da Pergunta 6 a um modelo GWR. Apresente os 
# resultados comparados (equação, R2). Qual modelo você escolheria como final? 
# Se desejar, apresente mapas que sustente sua justificativa.

# initial setup
res.palette <- colorRampPalette(c("red","orange","white","lightgreen","green"), 
                                space = "rgb")
pal <- res.palette(5)
par(mar = c(2, 0, 4, 0))

# GWR model (Geographically Weighted Regression)
target.gwr.multivaluated.sel <- gwr.sel(INDICE95 ~ 
                                          INDICE94 +
                                          GINI_91 + 
                                          URBLEVEL, 
                                        data = target, 
                                        coords = coords, 
                                        adapt = TRUE, 
                                        method = "aic",
                                        gweight = gwr.Gauss,
                                        verbose = TRUE)

target.gwr.multivaluated.model <- gwr(INDICE95 ~ 
                                        INDICE94 +
                                        GINI_91 + 
                                        URBLEVEL, 
                                      data = target, 
                                      coords = coords, 
                                      bandwidth = target.gwr.multivaluated.sel,
                                      gweight = gwr.Gauss,
                                      adapt = target.gwr.multivaluated.sel,
                                      hatmatrix = TRUE)

# calculate global residual SST (SQT)
SST <- sum((target$INDICE95 - mean(target$INDICE95)) ^ 2)
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
target.gwr.coefficients <- target.gwr.model$SDF$URBLEVEL

target.gwr.coefficients.classes_fx <- classIntervals(target.gwr.coefficients, n = 5, style="fixed", 
                                                     fixedBreaks=c(-.005,-.003,-.001,.001,.003,.005), 
                                                     rtimes = 1)
cols.gwr.coefficients <- findColours(target.gwr.coefficients.classes_fx, pal)

plot(target, col = cols.gwr.coefficients, main = "GWR Model (coefficients)", border = "grey")
legend(x = "bottom", cex = 1, fill = attr(cols.gwr.coefficients,"palette"), bty = "n",
       legend = names(attr(cols.gwr.coefficients, "table")),
       title = "Local Coefficient Estimates (urblevel)", ncol = 3)

moran.test(target.gwr.coefficients, listw = lw, zero.policy = T)